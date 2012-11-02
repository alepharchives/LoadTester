-record(job, 
	{
	  type             ::get|put,
	  name             ::atom(),
	  n_procs=1        ::pos_integer(),
	  url=""           ::string(),
	  headers=[]       ::[tuple()],
	  action_fn        ::function(),
	  last_response="" ::string(),
	  limit=1          ::pos_integer(),
	  delay_ms=0       ::non_neg_integer(),
	  count=0          ::non_neg_integer(),
	  min=999999999    ::non_neg_integer(),
	  max=0            ::non_neg_integer(),
	  avg=0            ::non_neg_integer(),
	  total_time=0     ::non_neg_integer(),
	  register         ::any()
	}).
