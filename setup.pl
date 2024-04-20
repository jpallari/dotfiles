#!/usr/bin/env perl
use warnings;
use utf8;
use strict;
use autodie;
use feature qw(say);
use open qw(:std :utf8);
use feature qw(signatures);
no warnings qw(experimental::signatures);

require Carp;
require Cwd;
require File::Copy;
require File::Basename;
require File::Path;
require File::Spec;

### Targets BEGIN ###

my @xdg_targets = (
    "nvim",
    "powershell",
    "wezterm",
);
my @dotfile_targets = (
    ["bashrc", "zshrc"],
    "bashrc",
    "inputrc",
    "screenrc",
    "tmux.conf",
    "Xresources",
);

### Targets END ###

my $home_dir          = $ENV{"HOME"};
my $xdg_config_dir    = $ENV{"XDG_CONFIG_HOME"} || File::Spec->catdir($home_dir, ".config");
my $xdg_data_dir      = $ENV{"XDG_DATA_HOME"} || File::Spec->catdir($home_dir, ".local", "share");
my $dotfile_dir       = File::Basename::dirname(Cwd::abs_path(__FILE__));

sub update_link {
    my ($dotfile_target_name, $fs_target_path) = @_;
    my $dotfile_target_path = File::Spec->catfile($dotfile_dir, $dotfile_target_name);

    unless (-e $dotfile_target_path) {
        Carp::croak("File " . $dotfile_target_path . " does not exist");
    }

    # If the file system path and the dotfile path resolve to same the path,
    # it means that we already have the link in place.
    if (Cwd::abs_path($fs_target_path) eq $dotfile_target_path) {
        say "Already linked: " . $fs_target_path . " => " . $dotfile_target_name;
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
        say "Already linked: gitconfig";
        return 1;
    }

    say "Linking: gitconfig";
    return system("git", "config", "--global", "--add", "include.path", $gitconfig_target);
}

sub main {
    foreach (@xdg_targets) {
        my $dotfile_target = $_;
        my $fs_target_path = File::Spec->catfile($xdg_config_dir, $dotfile_target);
        update_link($dotfile_target, $fs_target_path);
    }

    foreach (@dotfile_targets) {
        my $dotfile_target;
        my $fs_target;

        if (ref($_) eq 'ARRAY') {
            $dotfile_target = $_->[0];
            $fs_target      = "." . $_->[1];
        }
        else {
            $dotfile_target = $_;
            $fs_target      = "." . $_;
        }

        my $fs_target_path = File::Spec->catfile($home_dir, $fs_target);
        update_link($dotfile_target, $fs_target_path);
    }

    link_gitconfig();
}

main();
