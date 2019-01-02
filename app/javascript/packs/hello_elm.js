// Run this example by adding <%= javascript_pack_tag "hello_elm" %> to the
// head of your layout file, like app/views/layouts/application.html.erb.
// It will render "Hello Elm!" within the page.

import {
  Elm
} from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  const target = document.createElement('div');

  document.body.appendChild(target);
  const app = Elm.Main.init({
    node: target
  });

  App.cable.subscriptions.create(
    { channel: "GameChannel", game: "first-game" },
    {
      connected: function() {
        // Called when the subscription is ready for use on the server
        app.ports.setFromServer.send(1);
      },
      disconnected: function() {
        // Called when the subscription has been terminated by the server
        app.ports.setFromServer.send(3);
      },
      received: function(data) {
        console.log(data);
        app.ports.setFromServer.send(2);
        // Called when there's incoming data on the websocket for this channel
      }
    }
  );
})
