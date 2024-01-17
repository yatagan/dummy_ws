1. Build and run:
```bash
    $ make run
```

2. open http://localhost:8080/

3. Press button 'connection'

4. Send some heap binaries to the WS process
```erlang
(dummy_ws@127.0.0.1)1> leaky_ws:send_heap_bins(100000).
```

5. Observe the memory leak
```erlang
(dummy_ws@127.0.0.1)3> leaky_ws:process_infos().
```
