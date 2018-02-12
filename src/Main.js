'use strict'
exports.args =
  { bin : process.argv[0] || ""
  , command : process.argv[1] || ""
  }

exports._readJsonFromStdin = function (onError, onSuccess) {
    var data = ""
    process.stdin.on('data', function (chunk) {
        data += chunk.toString()
    })

    process.stdin.on('end', function () {
        var result
        try {
            result = JSON.parse(data)
        } catch (e) {
            return onError(e)
        }
        return onSuccess(result)
    })

    return function canceler (cancelError, cancelerError, cancelerSuccess) {
        return cancelerSuccess()
    }
}
