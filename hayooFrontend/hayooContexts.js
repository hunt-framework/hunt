[
{
    "cmd": "insert-context",
    "context": "function-description",
    "schema": {
        "weight": 1.0,
        "default": true,
        "normalizers": [],
        "regexp": "\\w*",
        "type": "text"
    }
}
,
{
    "cmd": "insert-context",
    "context": "function-module",
    "schema": {
        "weight": 0.5,
        "default": true,
        "normalizers": [],
        "regexp": "*",
        "type": "text"
    }
}
,
{
    "cmd": "insert-context",
    "context": "function-package",
    "schema": {
        "weight": 1.0,
        "default": true,
        "normalizers": [],
        "regexp": "\\w*",
        "type": "text"
    }
}
,
{
    "cmd": "insert-context",
    "context": "function-signature",
    "schema": {
        "weight": 1.0,
        "default": true,
        "normalizers": [],
        "regexp": "*",
        "type": "text"
    }
}
,
{ // don't add to index
    "cmd": "insert-context",
    "context": "function-source",
    "schema": {
        "weight": 0.1,
        "default": true,
        "normalizers": [],
        "regexp": "*",
        "type": "text"
    }
}
,
{
    "cmd": "insert-context",
    "context": "function-name",
    "schema": {
        "weight": 3.0,
        "default": true,
        "normalizers": [],
        "regexp": "*",
        "type": "text"
    }
}
]