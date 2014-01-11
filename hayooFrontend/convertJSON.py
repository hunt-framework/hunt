#!/usr/bin/env python3
import os
import glob
import json

__author__ = 'Sebastian'

_funcFolder = "../hayooCrawler/dist/build/hayooCrawler/functions/"
_outFile = "functions.js"

def main():
    if not os.path.isdir(_funcFolder):
        raise Exception("nothing to import")
    files = glob.glob(_funcFolder + "*/*.js")
    allFuncs = []
    for fName in files:
        with (open(fName)) as f:
            doc = json.load(f)[0]
            newDoc = {}
            newDoc["uri"] = doc["uri"]
            newDoc["description"] = {}
            
            if len(doc["description"]) > 7:
                raise Exception(str((doc["description"].keys())))
            newDoc["description"]["module"] = doc["description"]["fct-module"]
            newDoc["description"]["package"] = doc["description"]["fct-package"]
            newDoc["description"]["signature"] = doc["description"]["fct-signature"]
            newDoc["description"]["name"] = doc["description"]["title"]
            newDoc["description"]["type"] = doc["description"]["fct-type"]
            
            newDoc["index"] = dict(newDoc["description"])
            
            if "fct-source" in doc["description"]:
                newDoc["description"]["source"] = doc["description"]["fct-source"]
            if "fct-descr" in doc["description"]:
                newDoc["description"]["description"] = doc["description"]["fct-descr"]
            allFuncs.append(newDoc)
    with open(_outFile, 'wb') as allf:
        allf.write(bytes(json.dumps(allFuncs, indent=4), 'UTF-8'))

if __name__ == "__main__":
    main()