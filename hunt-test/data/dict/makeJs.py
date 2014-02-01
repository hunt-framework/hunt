#!/usr/bin/env python3
import os
import glob
import json
import sys
import argparse

__author__ = 'Sebastian'

_outFile = 'geoContexts.js'

def context(name, weight=1.0, default=True, normalizers=None, regexp="\\w*", type="text"):
    if not normalizers: normalizers = []
    return {
    "cmd": "insert-context",
    "context": name,
    "schema": {
        "weight": weight,
        "default": default,
        "normalizers": normalizers,
        "regexp": regexp,
        "type": type
    }
}

def wordToJson(w):
    return {
        "description": {
            "name": w,
        },
        "index": {
            "name": w,
        },
        "uri": "words://" + w
    }

def insertCommand(d):
    return {
        "cmd": "insert",
        "document": d
    }

def main():
    contexts = [context("name")]
    words = list(map(insertCommand, map(wordToJson, map(str.strip, open(sys.argv[1])))))

    with open(sys.argv[1] + ".js", 'wb') as allf:
        allf.write(bytes(json.dumps(contexts + words, indent=4), 'UTF-8'))

if __name__ == "__main__":
    main()