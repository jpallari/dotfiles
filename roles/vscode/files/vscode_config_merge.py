#!/usr/bin/env python

from __future__ import print_function
import json
import argparse
import collections
import sys
import re

COMMENT_RE = re.compile('^[ ]*//')

OPT_PARSER = argparse.ArgumentParser(
    description='Visual Studio Code configuration merger'
)
OPT_PARSER.add_argument(
    'filenames',
    metavar='FILE',
    type=str,
    nargs='+',
    help='Files to merge together.'
)
OPT_PARSER.add_argument(
    '--type', '-t',
    dest='conf_type',
    metavar='TYPE',
    type=str,
    choices=('keybindings', 'settings'),
    required=True,
    help='The config type to merge. Either "keybindings" or "settings"'
)
OPT_PARSER.add_argument(
    '--output', '-o',
    dest='output',
    metavar='FILE',
    type=str,
    required=False,
    help='Output file for the results. Default: stdout'
)

def join_keybindings(filenames):
    '''
    Join VSCode keybindings from the given files.
    '''
    aggr = []
    for filename in filenames:
        arr = read_json_file(filename)
        if isinstance(arr, list):
            aggr.extend(arr)
        else:
            raise Exception('Root element of %s is not an array' % filename)
    filtered = collections.deque()
    keys = set()
    for e in reversed(aggr):  # Dedupe starting from the end of the list
        key = e['key']
        if not key in keys:
            filtered.appendleft(e)
        keys.add(key)
    return list(filtered)

def join_settings(filenames):
    '''
    Join VSCode settings from the given files.
    '''
    aggr = {}
    for filename in filenames:
        d = read_json_file(filename)
        if isinstance(d, dict):
            dict_merge(aggr, d)
        else:
            raise Exception('Root element of %s is not an object' % filename)
    return aggr

def read_json_file(filename):
    '''
    Read JSON from the given file
    '''
    with open(filename) as filehandle:
        lines = filehandle.readlines()

    text = '\n'.join(
        line for line in lines
        if COMMENT_RE.match(line) is None
    )
    return json.loads(text)

def dict_merge(dct, merge_dct):
    '''
    Merge second dictionary into the first one.
    '''
    for k, _ in merge_dct.iteritems():
        if k in dct \
            and isinstance(dct[k], dict) \
            and isinstance(merge_dct[k], collections.Mapping):
            dict_merge(dct[k], merge_dct[k])
        else:
            dct[k] = merge_dct[k]

def write_as_json(obj, filename):
    '''
    Write object to given file as JSON.
    If no file is provided, write to STDOUT instead.
    '''
    if filename is None:
        json.dump(obj, sys.stdout, indent=4)
    else:
        with open(filename, 'w') as filehandle:
            json.dump(obj, filehandle, indent=4)

def main():
    '''
    Main program for this script
    '''
    opts = OPT_PARSER.parse_args()
    if opts.conf_type == 'keybindings':
        result = join_keybindings(opts.filenames)
    elif opts.conf_type == 'settings':
        result = join_settings(opts.filenames)
    else:
        raise Exception("Invalid config type: " + opts.conf_type)
    write_as_json(result, opts.output)

if __name__ == '__main__':
    main()
