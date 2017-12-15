# table-parser

[![Build Status](https://travis-ci.org/ChinaXing/table-parser.svg?branch=master)](https://travis-ci.org/ChinaXing/table-parser)

## usage

```
./table-parse  <<EOF
  create table a (
    id int,
    name varchar(256) not null default '_' comment '名字',
    primary key (`id`),
    UNIQUE KEY uni_name (`name`)
  )ENGINE=INNODB CHARSET=utf8mb4 COMMENT='测试';
EOF
```
