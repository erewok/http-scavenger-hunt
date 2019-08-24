# http-scavenger-hunt

```sh
  _   _ _____ _____ ____
 | | | |_   _|_   _|  _ \
 | |_| | | |   | | | |_) |
 |  _  | | |   | | |  __/
 |_____| |_|   |_| |_|
 / ___|  ___ __ ___   _____ _ __   __ _  ___ _ __
 \___ \ / __/ _` \ \ / / _ \ '_ \ / _` |/ _ \ '__|
  ___) | (_| (_| |\ V /  __/ | | | (_| |  __/ |
 |_____ \___\__,_| \_/ \___|_| |_|\__, |\___|_|
 | | | |_   _ _ __ | |_           |___/
 | |_| | | | | '_ \| __|
 |  _  | |_| | | | | |_
 |_| |_|\__,_|_| |_|\__|
```

Welcome to the HTTP Scavenger Hunt! This is a game meant to make it easier to learn about HTTP. Try to figure out how it works and be sure to identify yourself so we can count you!

## Getting Started

You will need some mechanism for issuing HTTP requests. To get started, visit the project root ([localhost:3000/](http://localhost:3000/) if running locally). It should have some suggestions for places to get started.

Figure out how to make HTTP requests of different endpoints using different methods and headers and you will rack up points for your team.

## Running the Project

This application relies on a Redis database for its backend. To run, set the following environment variables:

```sh
❯ cat .env
export ENVIRONMENT=local
export VERSION=1.0.0
export PORT=3000
export REDIS_HOST=localhost
export REDIS_PORT=6379
export REDIS_DB=3
export REDIS_PASSWD=
```

After that, you can build and run it with stack:

```sh
❯ stack build
http-scavenger-hunt-0.1.0.0: unregistering (local file changes: src/HttpHunt/Admin/Api.hs src/HttpHunt/Public/Api.hs)
...
❯ stack exec http-scavenger-hunt
ApiConfig {environment = Local, version = "1.0.0", port = 3000, redisHost = "localhost", redisPort = "6379", redisDb = "3", redisPasswd = Nothing}
*****The HTTP Scavenger Hunt is now running on Port 3000*****
```

To view the scoreboard and start the application, visit [localhost:3000/scoreboard](http://localhost:3000/scoreboard)
