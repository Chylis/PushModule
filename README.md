# PushModule
A simple push notification module written in Erlang, deployed on a ChicagoBoss Web server :-)

Notifications are sent through the Google Cloud Messaging API.

Features:

- A restful API for registering users and device tokens

- An admin interface that allows: 
  - Logging in/out, 
  - Creating and scheduling push notifications,
  - View scheduled notifications
  - View and trace sent notifications, e.g. sent date, payload, receivers, status for each dispatched notification, etc
  - View registered users, e.g. user info, received notifications, registered devices, device info, etc
