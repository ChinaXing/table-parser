CREATE TABLE `clue_bind_data` (
  `assign_source` TINYINT NOT NULL DEFAULT '1' COMMENT '分配来源: 1:系统分配 2:人工分配 3:自主认领',
  PRIMARY KEY (`id`),
  KEY `idx_mobile` (`mobile`) COMMENT '支持mobile的模糊查询',
  KEY `idx_name` (`name`) COMMENT '支持name模糊匹配'
) ENGINE=INNODB CHARSET=utf8mb4 collate = utf8mb4_bin COMMENT = 'ok';
