/*
Execute this script by running 'mysql -u<username> -p < push_module.sql'

*/

CREATE DATABASE push_module;

USE push_module;

CREATE TABLE counters (
    name                VARCHAR(255) PRIMARY KEY,
    value               INTEGER DEFAULT 0
    );

CREATE TABLE admins (
    id                  MEDIUMINT NOT NULL AUTO_INCREMENT,
    name                VARCHAR(255) NOT NULL,
    password_hash       VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
    );

CREATE TABLE notifications (
    id                  MEDIUMINT NOT NULL AUTO_INCREMENT,
    notification_template_id VARCHAR(255) NOT NULL,
    delivery_status     VARCHAR(255),
    gcm_message_id      VARCHAR(255),
    device_id           VARCHAR(255),
    PRIMARY KEY (id)
    );

CREATE TABLE usrs (
    id                  MEDIUMINT NOT NULL AUTO_INCREMENT,
    external_id         VARCHAR(255) NOT NULL,
    additional_info     VARCHAR(255),
    created_at          DATETIME NOT NULL,
    updated_at          DATETIME NOT NULL,
    PRIMARY KEY (id)
    );

CREATE TABLE devices (
    id                  MEDIUMINT NOT NULL AUTO_INCREMENT,
    device_id           VARCHAR(255) NOT NULL,
    usr_id              VARCHAR(255) NOT NULL,
    additional_info     VARCHAR(255),
    gcm_token           VARCHAR(255),
    created_at          DATETIME NOT NULL,
    updated_at          DATETIME NOT NULL,
    PRIMARY KEY (id)
    );

CREATE TABLE notification_templates (
    id                  MEDIUMINT NOT NULL AUTO_INCREMENT,
    admin_id            MEDIUMINT NOT NULL,
    title               VARCHAR(255) NOT NULL,
    body                VARCHAR(255) NOT NULL,
    status              VARCHAR(255),
    scheduled_for       DATETIME NOT NULL,
    sent_at             DATETIME,
    created_at          DATETIME NOT NULL,
    updated_at          DATETIME NOT NULL,
    PRIMARY KEY (id)
    );

/* Insert admin with username 'admin' and password 'admin' */
INSERT INTO admins (name, password_hash) VALUES ('admin', '86f3059b228c8acf99e69734b6bb32cc');
