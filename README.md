# Requirements

> go get -u gonum.org/v1/gonum/mat
> go get github.com/halturin/ergonode

# Build go

Unfortunately rebar does not build go progams

> go build go_src/gonum.go
> mkdir -p priv/bin
> mv gonum priv/bin

Now In a separate window, run

> priv/bin/gonum

# Build erlang

> rebar compile

# Run 

> erl -sname client -setcookie monster -pa ./ebin/

> matrix_gonum:version().

> matrix_gonum:transpose([[1.0,2.0],[3.0,4.0]]).


