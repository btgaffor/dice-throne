# App.cable.subscriptions.create "GameChannel",
#   connected: ->
#     # Called when the subscription is ready for use on the server
#     console.log("connected!")

#   disconnected: ->
#     # Called when the subscription has been terminated by the server

#   received: (data) ->
#     console.log("received: ", data)
#     # Called when there's incoming data on the websocket for this channel
