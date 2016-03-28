#!/bin/bash

for i in `seq 1 1501`;
do

  ##Register users:
  userId="user"$i
    if [ $i == 1 ]
    then
      name="Magnus"
      gender="male"
      msisdn="0701515567"
      address="Midsommarvägen123"
      email="magnus.eriksson@gmail.com"
    elif [ $i == 2 ]
    then
      name="Victor"
      gender="male"
      msisdn="0701513682"
      address="Hornsgatan523"
      email="virrepinnen.pinnen@gmail.com"
    elif [ $i == 3 ]
    then
      name="Henrietta"
      gender="female"
      msisdn="0763453682"
      address="Lindhagensgatan523"
      email="henrietta.holk@gmail.com"
    else
      name="Anonymous_user"
      gender=""
      msisdn=""
      address=""
      email=""
    fi

    additionalInfo='{"name":"'$name'","gender":"'$gender'","msisdn":"'$msisdn'","address":"'$address'","email":"'$email'"}'
    curl -v -d 'userId='$userId'&additionalInfo='$additionalInfo localhost:8001/user/register_user

##Register devices:
  for d in {1..2}; do
    deviceId=$userId"_device"$d
    gmToken=$userId"_device"$d"_token"$d

    if [ $d == 1 ]
    then
      os="iOS"
      osVersion="9.2"
      deviceType="iPhone6"
    else
      os="iOS"
      osVersion="8.4"
      deviceType="iPad"
    fi
    additionalInfo='{"os":"'$os'","osVersion":"'$osVersion'","deviceType":"'$deviceType'","appVersion":"1.0.1"}'
    curl -v -d 'userId='$userId'&deviceId='$deviceId'&additionalInfo='$additionalInfo localhost:8001/device/register_device
    curl -v -d 'token='$gmToken'&deviceId='$deviceId localhost:8001/device/register_token 
  done 

done 

#Register an actual token...
gcmToken="kRr7EgoyHmo:APA91bHJl6CUjZ6gd_HDzkA3P3-ybnfP0uEm9_83_FSFCUVZ1_5TVKVQO7QlnI_mMEQqOjOo3hgmSxi0cUQ5wDhxO-5tavhYOfxGeNWupW7tMzoMf8tXQd0puOfafNtMs1s6PyB10V3r"
deviceId="user1_device1"
#curl -v -d 'token='$gcmToken'&deviceId='$deviceId localhost:8001/device/register_token 
