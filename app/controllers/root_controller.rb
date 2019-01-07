class RootController < ApplicationController
  def index
    respond_to :html
  end

  def save
    game = Game.find_or_create_by(name: 'first-game')
    game.update(state: params[:game])

    puts "VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"
    puts ""
    puts params.to_unsafe_h
    puts ""
    puts "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"

    ActionCable.server.broadcast('game_first-game', game.state)

    # Just say that it was successful. The new game state will be returned via
    # the socket above
    head :ok
  end
end
