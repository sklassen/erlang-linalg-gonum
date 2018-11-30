package main

import (
	"flag"
	"fmt"
	"github.com/halturin/ergonode"
	"github.com/halturin/ergonode/etf"
)

// GenServer implementation structure
type goGenServ struct {
	ergonode.GenServer
	completeChan chan bool
}

var (
	SrvName   string
	NodeName  string
	Cookie    string
	err       error
	EpmdPort  int
	EnableRPC bool
)

// Init initializes process state using arbitrary arguments
func (gs *goGenServ) Init(args ...interface{}) interface{} {
	// Self-registration with name go_srv
	gs.Node.Register(etf.Atom(SrvName), gs.Self)

	// Store first argument as channel
	gs.completeChan = args[0].(chan bool)

	return nil
}

// HandleCast serves incoming messages sending via gen_server:cast
func (gs *goGenServ) HandleCast(message *etf.Term, state interface{}) (code int, stateout interface{}) {
	fmt.Printf("HandleCast: %#v (unhandled)", *message)
	stateout = state
	code = 0
	return
}

// HandleCall serves incoming messages sending via gen_server:call
func (gs *goGenServ) HandleCall(from *etf.Tuple, message *etf.Term, state interface{}) (code int, reply *etf.Term, stateout interface{}) {
	fmt.Printf("HandleCall: %#v, From: %#v (unhandled)\n", *message, *from)
	stateout = state
	code = 0
	return
}

// HandleInfo serves all another incoming messages (Pid ! message)
func (gs *goGenServ) HandleInfo(message *etf.Term, state interface{}) (code int, stateout interface{}) {
	fmt.Printf("HandleInfo: %#v\n", *message)
    stateout = state
    code = 0
    // Check type of message
    //var self_pid etf.Pid = gs.Self
    switch req := (*message).(type) {
        case etf.Tuple:
            if len(req) == 3 {
                from:=req[0].(etf.Pid)
                switch act := req[1].(type) {
                    case etf.Atom:
                        if string(act) == "ping" {
                            reply:=etf.Term(etf.Tuple{etf.Atom("ok"),etf.Atom("pong")})
                            gs.Send(from, &reply)
                        } else if string(act) == "version" {
                            reply:=etf.Term(etf.Tuple{etf.Atom("ok"),etf.Atom("version")})
                            gs.Send(from, &reply)
                        } else {
                            reply:=etf.Term(etf.Tuple{etf.Atom("error"),etf.Atom("unknown")})
                            gs.Send(from, &reply)
                        }
                    default:
                        reply:=etf.Term(etf.Tuple{etf.Atom("error"),etf.Atom("malformed")})
                        gs.Send(from, &reply)
                }
            }
        case etf.Atom:
            if string(req) == "stop" {
                // If message is atom 'stop', we should say it to main process
                gs.completeChan <- true
            }
    }
    return
}

// Terminate called when process died
func (gs *goGenServ) Terminate(reason int, state interface{}) {
	fmt.Printf("Terminate: %#v\n", reason)
}

func init() {
	flag.StringVar(&SrvName, "gen_server", "gonum", "gen_server name")
	flag.StringVar(&NodeName, "name", "gonum@localhost", "node name")
	flag.StringVar(&Cookie, "cookie", "monster", "cookie for interaction with erlang cluster")
	flag.IntVar(&EpmdPort, "epmd_port", 15151, "epmd port")
}

func main() {
	flag.Parse()

	// Initialize new node with given name and cookie
	n := ergonode.Create(NodeName, uint16(EpmdPort), Cookie)

	// Create channel to receive message when main process should be stopped
	completeChan := make(chan bool)

	// Initialize new instance of goGenServ structure which implements Process behaviour
	gs := new(goGenServ)

	// Spawn process with one arguments
	n.Spawn(gs, completeChan)

	fmt.Println("gonum interface")
	fmt.Printf("{%s,'%s'}!{self(),ping,[]}, flush().\n", SrvName, NodeName)
	fmt.Printf("{%s,'%s'}!stop.\n", SrvName, NodeName)

	// Wait to stop
	<-completeChan

	return
}