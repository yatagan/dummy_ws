## Tested with Erlang versions
* 22.3.4.26
* 23.3.4.18

## Steps to reproduce

1. Build and run:
```bash
make run
```

2. open http://localhost:8080/

3. Press button 'connection' to open a WS

4. Send some heap binaries to the WS process
```erlang
leaky_ws:send_heap_bins(100000).
```

5. Observe the memory leak
```erlang
leaky_ws:process_infos().
```
