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
>output

```

{"Right":[{"engine":"INNODB","collate":null,"name":"a","indices":[{"unique":false,"pk":true,"name":"PK","columns":["id"],"id":0,"comment":null},{"unique":t>rue,"pk":false,"name":"uni_name","columns":["name"],"id":0,"comment":null}],"charset":"utf8mb4","columns":[{"pk":false,"updateDefaultValue":null,"collate":null,"name":"id","charset":null,"id":0,"dataType":{"len":{"none":true},"type":"INT","unsigned":{"none":true}},"defaultValue":null,"comment":null,"autoIncrement":false,"nullAble":true},{"pk":false,"updateDefaultValue":null,"collate":null,"name":"name","charset":null,"id":0,"dataType":{"len":{"some":256},"type":"VARCHAR","unsigned":{"none":true}},"defaultValue":{"some":"_"},"comment":"名字","autoIncrement":false,"nullAble":false}],"comment":"测试"}]}
```
