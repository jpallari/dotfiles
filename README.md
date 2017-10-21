# dotfiles

Here's a bunch of my dotfiles. Some of them I've built myself and some of the
stuff is recycled from other people's files. Feel free to grab any part of
these configurations for your own use.

## Installation

Installation is done using [Ansible](https://www.ansible.com/).
Clone this repo and the following command on Linux:

    $ ansible-playbook -i hosts linux_playbook.yml

On OS X, run the following:

    $ ansible-playbook -i hosts osx_playbook.yml
