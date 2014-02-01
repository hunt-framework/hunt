#!/usr/bin/env python3
import os
import glob
import json

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

def main():
    contexts = [context("name"), context("kind", weight=0.1)] + list(map(lambda n: context(n,0.1), [
     "amenity"
    , "highway"
    , "historic"
    , "shop"
    , "tourism"
    , "leisure"
    , "website"

    , "place"
    , "memorial:type"
    , "contact:website"
    , "cuisine"
    , "operator"
    , "phone"])) + [context("position", type="position")]
    with open(_outFile, 'wb') as allf:
        allf.write(bytes(json.dumps(contexts, indent=4), 'UTF-8'))

if __name__ == "__main__":
    main()