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
        "regexp": "((%5C-%3F)(((%5B1-9%5D(%5B0-9%5D*))%3F)((%5B0-9%5D%7B4%2C4%7D)(%5C-(((0%5B1-9%5D)%7C(1%5B0-2%5D))(%5C-(((0%5B1-9%5D)%7C((%5B12%5D%5B0-9%5D)%7C(3%5B01%5D)))((Z%7C(%5B%5C%2B%5C-%5D((((0%5B0-9%5D)%7C(1%5B0-3%5D))(%3A(%5B0-5%5D%5B0-9%5D)))%7C(1(4(%3A(0(0(%3A(00))))))))))%3F))))))))",
        "type": "date"
    }
}
]
