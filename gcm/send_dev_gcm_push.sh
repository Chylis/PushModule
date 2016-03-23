apiKey='AIzaSyBWdyC1eyn5ohraoQp1oPK83SIG73WAX_Q'

curl -H "Content-Type:application/json" -H "Authorization:key=$apiKey" -X POST -d @gcm-dev-payload.json https://gcm-http.googleapis.com/gcm/send
