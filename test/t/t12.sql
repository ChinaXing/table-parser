 CREATE TABLE `goods_v2` (
 `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '页面ID',
 `sub_title` varchar(100) DEFAULT '''' COMMENT '子标''题',
 `snap_key` varchar(50) NOT NULL DEFAULT '' COMMENT '当前商"品镜像的key',
  primary key (`id`),
  KEY `idx_snap_key` (`snap_key`(33), `sub_title`) COMMENT 'ok'
) ENGINE = InnoDB CHARSET = utf8mb4;
