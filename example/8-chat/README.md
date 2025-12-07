# Chat Room Example

A multi-module chat room demonstrating cross-module function calls and actor-based architecture.

## Modules

- **message.ml** - Message types and formatting (pure data module)
- **user.ml** - User actor with inbox and room membership
- **room.ml** - Room actor managing members and message history
- **chat_server.ml** - Main entry point orchestrating the demo

## Build & Run

```bash
make clean && make run
```

## What It Demonstrates

1. **Cross-module function calls** - `Room.start`, `User.send`, `Message.create`
2. **Actor-based architecture** - Each user and room is an independent actor
3. **Message passing** - Users send messages to rooms, rooms broadcast to members
4. **State management** - Room tracks members and history, users track inbox

## Output

```
=== Chat Room Demo ===

Creating room 'general'...
Creating users...

Users joining the room...
Room general: user joined
Alice joined a room
...

Sending messages...
Room general: broadcasting from Alice
Bob received: [Alice] Hello everyone!
...

=== Final Status ===
Members remaining: 2
Total messages: 7

=== Demo Complete ===
```
