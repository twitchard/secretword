'use strict'
const DynamoDB = require('aws-sdk/clients/dynamodb')
exports._getClient = function (options) {
    return new DynamoDB.DocumentClient(options)
}

exports._loadRecord = function (dynamodb) {
    return function (tableName) {
        return function (key) {
            return function (onError, onSuccess) {
                dynamodb.get({
                    TableName: tableName,
                    Key: key
                }, function (err, data) {
                    if (err) {
                        return onError(err)
                    }
                    return onSuccess(data)
                })
                return function canceler() {

                }
            }
        }
    }
}

exports._saveRecord = function (dynamodb) {
    return function (tableName) {
        return function (item) {
            return function (onError, onSuccess) {
                dynamodb.put({
                    TableName: tableName,
                    Item: item
                }, function (err) {
                    if (err) return onError()

                    return onSuccess()
                })
                // TODO?
                return function canceler() {
                }
            }
        }
    }
}

exports._deleteRecord = function (dynamodb) {
    return function (tableName) {
        return function (key) {
            return function (onError, onSuccess) {
                dynamodb.delete({
                    TableName: tableName,
                    Key: key
                }, function (err) {
                    if (err) return onError()

                    return onSuccess()
                })
                // TODO?
                return function canceler() {
                }
            }
        }
    }
}
