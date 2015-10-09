
reqs = client init_ resps
resps = server reqs

-- bad variant because of recursive function
-- client init_ (resp:resps) 	= init_ : client (next resp) resps

-- good but unreadable
-- client init_ resps 			= init_ : client (next head(resps)) (tail resps)

-- good and readable
client init ~(resp:resps) 	= init_ : client (next resp) resps

server 		(req:reqs) 		= process req : server reqs

init_ 		= 0
next resp 	= resp
process req = req + 1

main = do {
	print $ take 10 reqs;
}