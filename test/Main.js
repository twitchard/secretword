exports.log = function (s) {
    return function () {
        console.log(s)
    }
}
exports.test1 = {
  "session": {
    "new": false,
    "sessionId": "SessionId.404cf881-26e8-4b8e-be21-ce85902d69c9",
    "application": {
      "applicationId": "amzn1.ask.skill.fe3eeb2f-f7eb-4be5-bcd6-076532ec60cb"
    },
    "attributes": {},
    "user": {
      "userId": "foobar"
    }
  },
  "request": {
    "type": "IntentRequest",
    "requestId": "EdwRequestId.ac58f8bf-a834-4381-9598-ba9ca054f658",
    "intent": {
      "name": "ThinkingIntent",
      "slots": {}
    },
    "locale": "en-US",
    "timestamp": "2018-01-04T03:37:06Z"
  },
  "context": {
    "AudioPlayer": {
      "playerActivity": "IDLE"
    },
    "System": {
      "application": {
        "applicationId": "amzn1.ask.skill.fe3eeb2f-f7eb-4be5-bcd6-076532ec60cb"
      },
      "user": {
        "userId": "foobar"
      },
      "device": {
        "supportedInterfaces": {}
      }
    }
  },
  "version": "1.0"
}
