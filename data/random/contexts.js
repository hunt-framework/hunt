[
{
    "cmd": "insert-context",
    "context": "id",
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
    "context": "context1",
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
    "context": "context2",
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
    "context": "contextdate",
    "schema": {
        "weight": 1.0,
        "default": true,
        "normalizers": [],
	"regexp": "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))",
        "type": "date"
    }
}
]
