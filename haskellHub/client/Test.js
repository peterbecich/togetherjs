// Create WebSocket connection.
const socket = new WebSocket('ws://localhost:8081');

// Connection opened
socket.addEventListener('open', function (event) {
  console.log("Connection open");
  socket.send('Hi! My name is Peter');
});

// Listen for messages
socket.addEventListener('message', function (event) {
  console.log('Message from server:', event.data);
});

socket.addEventListener('close', function (event) {
  console.log('Connection closed:', event.data);
});
