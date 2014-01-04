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
            newDoc["description"]["type"] = "function"
            if len(doc["description"]) > 6:
                raise Exception(str((doc["description"].keys())))
            if "fct-descr" in doc["description"]:
                newDoc["description"]["function-description"] = doc["description"]["fct-descr"]
            newDoc["description"]["function-module"] = doc["description"]["fct-module"]
            newDoc["description"]["function-package"] = doc["description"]["fct-package"]
            newDoc["description"]["function-signature"] = doc["description"]["fct-signature"]
            newDoc["description"]["function-name"] = doc["description"]["title"]
            newDoc["index"] = dict(newDoc["description"])
            if "fct-source" in doc["description"]:
                newDoc["description"]["function-source"] = doc["description"]["fct-source"]
            allFuncs.append(newDoc)
    with open(_outFile, 'wb') as allf:
        allf.write(bytes(json.dumps(allFuncs, indent=4), 'UTF-8'))

if __name__ == "__main__":
    main()