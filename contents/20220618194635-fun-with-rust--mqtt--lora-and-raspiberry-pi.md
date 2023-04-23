Recently, I was called to give a hand on a small project for fun.
It was about tracking groups of people using GPS.

The project was nice and all, problem was trying to get the correct,
and simplier, tools for the project.

We found a company that helped us with the hardware stuff
and they provide us with some sensors (Dragino LGT-92)
and a gateway (Dragino LIG-16). They talk to each other using
radio.

The LIG-16 is a gateway that provides a lot of stuff
and we used its MQTT client to communicate with our broker
(since there was no connection at the location where it was used).

The first idea we tried was to have everything inside a iPhone,
but unfortunately we did not found an easy MQTT broker to use internally
with the app.

Than, we accepted we need to add an extra device to handle the application.
So, we added a Raspiberry PI 4 where we could run the MQTT broker and
provide a REST api to be consumed on a offline map on the phone.

This is the application we build:

[ ![application diagram](/images/gps-diagram.png "application diagram") ](/images/gps-diagram.png)

I wrote a rust application that with [rumqtt](https://github.com/bytebeamio/rumqtt) that provided
the borker and client for the MQTT and [warp](https://github.com/seanmonstar/warp) for the web server,
and [sqlx](https://github.com/launchbadge/sqlx) for the database (no need to orm or anything like,
all queries were simple).

<< Add images >>

To help with some message decoding I wrote a small library
[rust_dragino_lgt92_tools](https://github.com/diasbruno/rust_dragino_lgt92_tools)
