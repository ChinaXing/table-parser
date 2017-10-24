CREATE TABLE `zandb_host_job_ref` (
	  `id` int(11) NOT   NULL AUTO_INCREMENT COMMENT '主键',
	  `host_id` int(11) NOT NULL DEFAULT '-1'    COMMENT    'mysql_backup_host主键'   ,
	  `job_id` int(11) NOT NULL DEFAULT '0' COMMENT '任务id',
	  `is_enabled` tinyint(4) NOT NULL DEFAULT '1' COMMENT   '是否启用，1是，2否',
	  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
	  `update_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
	  `job_policy` time NOT NULL DEFAULT '23:59:59' COMMENT '任务每天的执行时间',
	  `last_exec_time` datetime NOT NULL DEFAULT '1900-01-01 00:00:00' COMMENT '上一次任务执行时间',
	  PRIMARY KEY (`id`),
	  UNIQUE KEY `uniq_hid_jid`(`host_id`,`job_id`)
) ENGINE=InnoDB AUTO_INCREMENT=577 DEFAULT CHARSET=utf8mb4 COMMENT='任务主机关系表'
