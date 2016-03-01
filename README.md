# throttle
Experimental implementation of a simple throttle

Initialize with: throttle:start_link(Fun_todo, [Fun_drop], Limit)

The idea is very simple: it starts two processes, Throttle and Worker. When you call throttle:send_job/1, which is the
only API method, it does the following:

* T receives your message (async)
* T sends it to W and increments counter by 1
* W does the job (meaning: executes the Fun_todo)
* W calls T to let it know that it is done
* T decrements the counter

Now, if counter exceeds Limit, T does not send job to W, but instead calls Fun_drop (which defaults to a no-op).

That's all.
