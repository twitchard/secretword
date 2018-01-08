exports.log = function (s) {
    return function () {
        console.log(s)
    }
}

exports.lambdaTest = function () {
    const DynamoDB = require('aws-sdk/clients/dynamodb')
    const client = new DynamoDB.DocumentClient({
        region: "us-east-1"
    })
    client.get({
        Key: { userId : "yikes"},
        TableName: "secretword__Sessions"
    }, function (err, data) {
        console.log(err, data)
    })
}

exports.test1 = {
  "session": {
    "new": false,
    "sessionId": "SessionId.404cf881-26e8-4b8e-be21-ce85902d69c9",
    "application": {
      "applicationId": "amzn1.ask.skill.fe3eeb2f-f7eb-4be5-bcd6-076532ec60cb"
    },
    "attributes": null,
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

exports.test2 = {
	"version": "1.0",
	"session": {
		"new": true,
		"sessionId": "amzn1.echo-api.session.f9703fd5-2abc-4c6b-9505-94b8be6e916c",
		"application": {
			"applicationId": "amzn1.ask.skill.fe3eeb2f-f7eb-4be5-bcd6-076532ec60cb"
		},
		"user": {
			"userId": "amzn1.ask.account.AEDCFKJK7QX3JRSZSUZC5WBYB5M3DKNVIP7EQLMC5EHHMUXTJZMV4NR2AWBP4N57NCUHY27KQ4KAAOUPEYAQLW46EUNZWB7U2CKBXY74GYPQP6MSAS3XIPJ2CUCVWW4WMH246GSM5DQHK3EZ5Z56FQBZMF6CR7P2JHGXXIJOQ7ZZ4BJAU3R6JUAPN3NCZTQHRGWZV4JV6VU6ODA"
		}
	},
	"context": {
		"AudioPlayer": {
			"playerActivity": "IDLE"
		},
		"Display": {
			"token": ""
		},
		"System": {
			"application": {
				"applicationId": "amzn1.ask.skill.fe3eeb2f-f7eb-4be5-bcd6-076532ec60cb"
			},
			"user": {
				"userId": "amzn1.ask.account.AEDCFKJK7QX3JRSZSUZC5WBYB5M3DKNVIP7EQLMC5EHHMUXTJZMV4NR2AWBP4N57NCUHY27KQ4KAAOUPEYAQLW46EUNZWB7U2CKBXY74GYPQP6MSAS3XIPJ2CUCVWW4WMH246GSM5DQHK3EZ5Z56FQBZMF6CR7P2JHGXXIJOQ7ZZ4BJAU3R6JUAPN3NCZTQHRGWZV4JV6VU6ODA"
			},
			"device": {
				"deviceId": "amzn1.ask.device.AHRNQO36Z62F3GKSE7JGEUYAFJ74XZZ2YZKT2UZIRJXYN6BUJOZO6PE7TLJQ3ZPZ25CUCMBGA2EJYFVGL5E5BPNAP76OLHR53IOJ2BLESJGBA3MK3BXNB4W6PR23UOZ2N7R4L2Q2472NRMCU2B3BMSJCRQRA",
				"supportedInterfaces": {
					"AudioPlayer": {},
					"Display": {
						"templateVersion": "1.0",
						"markupVersion": "1.0"
					}
				}
			},
			"apiEndpoint": "https://api.amazonalexa.com",
			"apiAccessToken": "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLmZlM2VlYjJmLWY3ZWItNGJlNS1iY2Q2LTA3NjUzMmVjNjBjYiIsImV4cCI6MTUxNTM3MTg3MywiaWF0IjoxNTE1MzY4MjczLCJuYmYiOjE1MTUzNjgyNzMsInByaXZhdGVDbGFpbXMiOnsiY29uc2VudFRva2VuIjpudWxsLCJkZXZpY2VJZCI6ImFtem4xLmFzay5kZXZpY2UuQUhSTlFPMzZaNjJGM0dLU0U3SkdFVVlBRko3NFhaWjJZWktUMlVaSVJKWFlONkJVSk9aTzZQRTdUTEpRM1pQWjI1Q1VDTUJHQTJFSllGVkdMNUU1QlBOQVA3Nk9MSFI1M0lPSjJCTEVTSkdCQTNNSzNCWE5CNFc2UFIyM1VPWjJON1I0TDJRMjQ3Mk5STUNVMkIzQk1TSkNSUVJBIiwidXNlcklkIjoiYW16bjEuYXNrLmFjY291bnQuQUVEQ0ZLSks3UVgzSlJTWlNVWkM1V0JZQjVNM0RLTlZJUDdFUUxNQzVFSEhNVVhUSlpNVjROUjJBV0JQNE41N05DVUhZMjdLUTRLQUFPVVBFWUFRTFc0NkVVTlpXQjdVMkNLQlhZNzRHWVBRUDZNU0FTM1hJUEoyQ1VDVldXNFdNSDI0NkdTTTVEUUhLM0VaNVo1NkZRQlpNRjZDUjdQMkpIR1hYSUpPUTdaWjRCSkFVM1I2SlVBUE4zTkNaVFFIUkdXWlY0SlY2VlU2T0RBIn19.Y_k6yWl_GPCb1dlwnnA3DcAKJNifJG6MRaFwHCz8-Ng7X8eBYI-irQVlOie9zA6SsdJM5X5lrLneWg5FAE5ukju_49SPXGQWuFHoOmGDCvPQV2rOHehte3TicBh0jMHMY3RJlHue3vIvIMPYhnelvuY96BX6QbCiTY8qSq607-SuvxJZtfs3U7W35Kr-zUaIMvp1EcRb9Voic45MOrFBy4p9shH5IdVdBRV7Fl9wVd6Jq5wn5JFcF0cuoUDsCBDbkUXHv9ePe3TYOx0Wd3Q_xbQkLJQSBzGPhFDzuf_CL-xhbGHR9QxrosDwdoc3sU1-_dDnWB3XI6fUZIpcwJEbAA"
		}
	},
	"request": {
		"type": "LaunchRequest",
		"requestId": "amzn1.echo-api.request.8d5730f6-0e10-4682-be13-153823b630b9",
		"timestamp": "2018-01-07T23:37:53Z",
		"locale": "en-US"
	}
}
