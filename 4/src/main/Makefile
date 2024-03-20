# example clients for demo

compile:
	erlc ../broadcaster/broadcaster.erl ../client/client.erl ../server/server.erl main.erl

s: compile
	mkfifo /tmp/fifo_dog || true
	erl -mnesia dir '"/tmp/fp_dog"' -noshell -s main main 1337 # < /tmp/fifo_dog &
	#echo "localhost 1337" > /tmp/fifo_dog
	#echo "dog" > /tmp/fifo_dog
	#cat > /tmp/fifo_dog

c: compile
	mkfifo /tmp/fifo_eva || true
	erl -mnesia dir '"/tmp/fp_eva"' -noshell -s main main 1330 < /tmp/fifo_eva &
	echo "localhost 1337" > /tmp/fifo_eva
	echo "eva" > /tmp/fifo_eva
	cat > /tmp/fifo_eva

c2: compile
	mkfifo /tmp/fifo_john || true
	erl -mnesia dir '"/tmp/fp_john"' -noshell -s main main 1331 < /tmp/fifo_john &
	echo "localhost 1337" > /tmp/fifo_john
	echo "john" > /tmp/fifo_john
	cat > /tmp/fifo_john

c3: compile
	mkfifo /tmp/fifo_lily || true
	erl -mnesia dir '"/tmp/fp_lily"' -noshell -s main main 1333 < /tmp/fifo_lily &
	echo "localhost 1330" > /tmp/fifo_lily
	echo "lily" > /tmp/fifo_lily
	cat > /tmp/fifo_lily

