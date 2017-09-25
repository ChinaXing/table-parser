CREATE TABLE `mid_track_event` (
	  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'key',
	  `youzan_app_id` varchar(32)  NOT NULL COMMENT '有赞业务标识',
	  `sdk_type` varchar(10) NOT NULL COMMENT 'iOS/Android/weapp/js等',
	  `event_type` varchar(20)  NOT NULL COMMENT '事件类型',
	  `event_sign` varchar(200)  NOT NULL COMMENT '事件标识,display事件特殊处理，会将page_type设为event_sign',
	  `event_name` varchar(200)  NOT NULL COMMENT '事件/页面名称',
	  `first_visit_time` varchar(20)  NOT NULL COMMENT '首次访问时间',
	  `last_visit_time` varchar(20)  NOT NULL COMMENT '最后访问时间',
	  `touch_count` bigint(20)  NOT NULL COMMENT '触发次数',
	  PRIMARY KEY (`id`),
	  UNIQUE KEY `idx_event_type` (`youzan_app_id`,`sdk_type`,`event_type`),
	  KEY `idx_event_sign` (`youzan_app_id`,`sdk_type`,`event_sign`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='日志统计事件类型中间表';
