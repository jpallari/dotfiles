#!/usr/bin/env perl
use warnings;
use strict;
use autodie;
use feature qw(say);

require Carp;
require Cwd;
require File::Copy;
require File::Basename;
require File::Path;
require File::Spec;

### Targets BEGIN ###

my @xdg_targets     = ("nvim",);
my @dotfile_targets = ("bashrc", "emacs.d", "inputrc", "screenrc", "tmux.conf", "vim", "Xresources",);

### Targets END ###

my $home_dir          = $ENV{"HOME"};
my $xdg_config_dir    = $ENV{"XDG_CONFIG_HOME"} || File::Spec->catdir($home_dir, ".config");
my $xdg_data_dir      = $ENV{"XDG_DATA_HOME"} || File::Spec->catdir($home_dir, ".local", "share");
my $dotfile_dir       = File::Basename::dirname(Cwd::abs_path(__FILE__));
my $vimplug_uri       = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim";
my $vimplug_path      = File::Spec->catdir($home_dir, ".vim", "autoload", "plug.vim");
my $nvim_autoload_dir = File::Spec->catdir($xdg_data_dir, "nvim", "site", "autoload");
my $nvimplug_path     = File::Spec->catfile($nvim_autoload_dir, "plug.vim");

sub update_link {
    my ($dotfile_target_name, $fs_target_path) = @_;
    my $dotfile_target_path = File::Spec->catfile($dotfile_dir, $dotfile_target_name);

    unless (-e $dotfile_target_path) {
        Carp::croak("File " . $dotfile_target_path . " does not exist");
    }

    # If the file system path and the dotfile path resolve to same the path,
    # it means that we already have the link in place.
    if (Cwd::abs_path($fs_target_path) eq $dotfile_target_path) {
        say "Already linked: " . $dotfile_target_path;
        return 1;
    }

    # Links to other places will be deleted.
    # Existing files will be backed up.
    if (-l $fs_target_path) {
        unlink($fs_target_path);
    }
    elsif (-e $fs_target_path) {
        rename($fs_target_path, $fs_target_path . ".bak");
    }

    # Link!
    say "Linking: " . $fs_target_path . " => " . $dotfile_target_path;
    symlink($dotfile_target_path, $fs_target_path);
    return 1;
}

sub link_gitconfig {
    my $gitconfig_target     = File::Spec->catfile($dotfile_dir, "gitconfig");
    my $current_git_includes = `git config --global --get-all include.path`;

    if (index($current_git_includes, $gitconfig_target) >= 0) {
        say "Already linked: " . $gitconfig_target;
        return 1;
    }

    say "Linking: " . $gitconfig_target;
    return system("git", "config", "--global", "--add", "include.path", $gitconfig_target);
}

sub setup_vimplug {
    unless (-f $vimplug_path) {
        say "Downloading vimplug from " . $vimplug_uri;
        system("curl", "-sSfLo", $vimplug_path, "--create-dirs", $vimplug_uri);
    }
    unless (-f $nvimplug_path) {
        say "Setting up vimplug for NVim";
        File::Path::make_path($nvim_autoload_dir);
        File::Copy::copy($vimplug_path, $nvimplug_path);
    }
}

sub main {
    foreach (@xdg_targets) {
        my $dotfile_target = $_;
        my $fs_target_path = File::Spec->catfile($xdg_config_dir, $dotfile_target);
        update_link($dotfile_target, $fs_target_path);
    }

    foreach (@dotfile_targets) {
        my $dotfile_target = $_;
        my $fs_target_path = File::Spec->catfile($home_dir, "." . $dotfile_target);
        update_link($dotfile_target, $fs_target_path);
    }

    link_gitconfig();
    setup_vimplug();
}

main();
