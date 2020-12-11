# DISCLAIMER 

**THIS IS ALPHA SOFTWARE. IT HAS ONLY BEEN TESTED FOR MINIMUM FUNCTIONALITY. THERE ARE ASSUREDLY BUGS LURKING IN THE CODE.**

**THE AUTHOR IS NOT RESPONSIBLE FOR ANY DAMAGE THAT THE PROGRAM MAY CAUSE. THIS SOFTWARE IS PROVIDED FOR EDUCATIONAL USE ONLY.**

Having said that, in all likelihood the worst thing that will happen is some space leak I'm unaware of or sending malformed packets.

If anyone would like to contribute to the development of Pλacket, open an issue and I'll get in touch. At the moment Pλacket is at the proof-of-concept stage, but I intend to keep developing it (if time allows) in order to support more protocols and features. 

At the moment, Pλacket supports **Ethernet, ARP, IPv4, ICMP(v4), TCP, UDP, and DNS** protocols.

# Table of Contents 

### I. [Pλacket Overview](#Pλacket)

### II. [Installation](#Installation)

### III. [Packet Machines](#Machines)

###  IV. [Record Syntax](#Records)

### V. [Packet Machines](#Machines)

### VI. [Packet Sources](#Sources)

### VII. [Commands and General Usage](#Commands)



# Pλacket

Pλacket (or placket if you hate Greek letters) is a command-line utility, written in Haskell and based on libpcap, that interprets a small reactive scripting language which facilitates generating and processing streams of raw network packets.

Pλacket allows the user to define and combine a variety of *sources* (e.g. network devices, pcap files, or packet generating functions), connect those sources to *packet machines*, which are workers that modify, filter, or perform effectful actions (such as sending, writing to a pcap file, pretty printing packets or fields to the terminal or a file, etc). 

Pλacket's packet machines are designed to be run **concurrently**. Additionally, assuming that GHC (the Glasgow Haskell Compiler) supports the -threaded option on your system, Pλacket should make effective use of multiple cores/threads. 


In this documentation, it will sometimes be useful to refer to the *shape* of a packet. The shape of a packet is the set of protocols, represented in descending order from left to right. For example, a typical DNS packet might have the shape [DNS ; UDP ; IP4 ; ETH].

# Installation 
Pλacket *should* be compatible with any modern Linux distribution. Windows is not supported at this time. (It may be possible to build Pλacket on MacOS, but this has not been tested.)

At the moment, the only way to install Pλacket is to compile it using the stack build tool. You can find instructions for installing stack [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux)

Pλacket requires two external dependencies: libtinfo and libpcap-dev

If you do not have these libraries present, they can be installed on Ubuntu with:

```
sudo apt install libtinfo
```

```
sudo apt install libpcap-dev
``` 

(Replace `sudo apt install` with `sudo dnf install` for Fedora).

If your distribution's repository does not contain libtinfo, you might try installing the ncurses-compat-libs package, which should satisfy the dependency. 

After installing the dependencies, clone this repository with:

```
git clone https://github.com/gnumonik/placket.git
```

Change directories: 

```
cd ./placket
```

And build Pλacket with the command:

```
stack build
```

*Note*: Compiling Pλacket might take a while, especially if you don't have (the needed versions of) any of the Haskell dependencies required.

*Note*: Stack will download the appropriate version of the GHC Haskell compiler. The Haskell compiler is quite large, but after compiling you may delete it without any negative consequences. 

Upon successful compilation of Pλacket, you should see a line of output that looks like:

```
Installing executable placket in </PATH/TO/BINARY>
```

The binary will be located in the `</PATH/TO/BINARY>` folder, and you may move it to a more suitable location if you like. 

**IMPORTANT**: Because Pλacket depends on libpcap, you must either run it as root or set the CAP_NET_RAW and CAP_NET_ADMIN capabilities for your user account on your system. I **strongly** recommend that that you do not run Pλacket as root and instead take the capabilities approach. (Consult google for specific information on setting capabilities for your system.)






# Record Syntax 
Each of the machines uses a variant of Pλacket's **record syntax** to access (and possibly modify) fields in a given packet. The record syntax varies slightly with the function of the machine (e.g. only machines that filter packets based on some field have employ the boolean comparison operators `>= / <= etc`), but the basic idea is the same.

The general format for record syntax is: 

```
<PROTOCOLTYPE> ( <FIELD1>.<SUBFIELD>.<SUBSUBFIELD>=<VALUE1> <FIELD2>.<SUBFIELD2>.<SUBSUBFIELD2>=<VALUE2> (etc) )
``` 

For machines that perform boolean tests on fields, the `=` maybe be replaced with `>=`,`<=`,`>`,`<`. The `=` operator is overloaded; in a context of setting packet fields, it means "set this field to the value on the right", whereas in a context of performing boolean tests it means "True if the field is equal to the value on the right".

In this documentation, a context that uses record syntax to modify or construct fields will be referred to as a **Record Builder**, while a context that uses record syntax to perform boolean tests will be known as a **Record Selector**. The sub-expression `<FIELD1>.<SUBFIELD>=<VALUE>` will be referred to as a **Field Builder** or **Field Selector** depending upon the context. 

### **Record Builders** 

The parentheses in record syntax are not optional, but the fields in between them may be omitted. In the **Record Builder** context, a lack of fields between the parentheses will construct a protocol message with *all default values*. Because Pλacket aims to give maximum control over the construction of raw packets, the default values are **not** "sensibly chosen". For all numerical fields, the default value is 0. For all flags, the default value is False or Off. For all ByteString fields, the default value is the empty bytestring. A protocol message that contains optional fields (fields that might have zero or more members) constructed with the default values will be set with 0 members. 

Furthermore, in the **Record Builder** context only, all numerical (or number-like fields) can be **enumerated** with a dash between the start value and the end value. The only field types that cannot be enumerated are Flags and ByteStrings. For example, the expression:

```
IP4 (src=0.0.0.0-0.0.0.10)
```

will construct 11 IPv4 messages. The first will have a source IP address of 0.0.0.0, while the last will have a source IP address of 0.0.0.10. 

**Warning:** use of multiple enumerations at the same time can cause the number of packets generated to grow exponentially. Though I have done my best to exploit Haskell's laziness to make generating large numbers of packets efficient, certain machines (particularly those that perform IO, like `send` or `dump`) will consume large amounts of memory if extremely high numbers of  packets are generated. 

In addition to enumeration, one can construct a **non-continguous set** of values with a record builder by enclosing multiple values, separated by commas, between `<` and `>`. The expression:

```
IP4 (src=<192.168.2.30, 192.168.2.50>)
``` 

constructs two IP4 protocol messages, one with a source of 192.168.2.30 and another with a source of 192.168.2.50

### **Record Selectors** 

As mentioned above, basic **Record Selector** syntax is nearly identical to the **Record Builder** syntax, differing only in  that `=` means "True if the field in the packet passed to the machine matches the value on the right", that the operators `>= <= > <` can replace `=`, and that enumerations and non-contiguous sets are not allowed.

More advanced record selectors can be constructed by combining Record Selectors and their component Field Selectors using boolean operators using a notation that is very similar to the BPF syntax to create compound truth functional expressions. Both Field Selectors (within the parentheses) and Record Selectors (the whole expression containing the Protocol Type and its associated Field Selectors) can be combined using the operators `||` ('or'), `&&` ('and'), `not` ('not'). 

Some examples:

The expression: 

```
ARP (op=1 || op=2)
``` 

will select any packet containing an ARP message with an opcode of 1 or 2. 

Similarly, the expression:

```
ARP (sha=ff:ff:ff:ff:ff:ff && (op=1 || op=2))
``` 

Selects any packet containing an ARP message with a broadcast sender address and either an opcode of 1 or 2. 

### **Field Value Format** 

All numerical fields can be entered either as decimal numbers, binary numbers, or hexadecimal numbers. By default, Pλacket will attempt to read the value as a decimal. To enter a number in binary or hex, simply prefix the number with `0b` for binary or `0x` for hex. For example, Each of the following expressions will set the etherType field of the ethernet message in a packet to 666.

```
set ETH (etherType=666)
```

```
set ETH (etherType=0b1010011010)
```

```
set ETH (etherType=0x029A)
```

Fields of the ByteString type can either be entered using an ascii string enclosed within quotation marks:

```
"thisIsAByteString"
```

 or in hex (the 0x prefix is not necessary here): 

 ```
 1337dadface
 ```

# Machines 

User defined machines are constructed by combining and configuring the built-in machines provided by Pλacket. 

The syntax for defining a machine is: `m: <NAME> = <MACHINE> `, where `<NAME>` is the name of the machine (only alphanumeric characters and underscores are allowed), and  `<MACHINE>` is either a a built-in machine, a user machine, or a combination (or composition) of built-in and/or user-defined machines.

There are two primitive operators for combining machines:

### (**~>**): `<MACHINE1> ~> <MACHINE2>`

This operator composes `<MACHINE1>` and `<MACHINE2>` to form a composite machine, which can be reused in the construction of other machines or run directly. When an expression of the form `<MACHINE1> ~> <MACHINE2>` is compiled, the resulting composite machine first applies `<MACHINE1>` and then feeds its input into `<MACHINE2>`.

The `~>` operator can be used to chain machines together *ad infinitum*. Any expression of the form `<MACHINE1> ~> <MACHINE2> ~> (...) ~> <MACHINEX>` will compile to a single machine, which can be reused.

### (**~+>**): `<MACHINE1> ~+> (<MACHINE2> ~+>  <MACHINE3>) (<MACHINE4>) (...)` 

The `~+>` operator is the *fanout* operator. It takes the output of the machine on the left, and feeds it into each of the machines on the right. The composite machine produced by the fanout operator yields the output of each of the machines on the right.

Note that a machine definition that employs the `~+>` operator must enclose every independent machine to the right of the `~+>` with parentheses. The sequence of machines in parentheses following the `~+>` is final, i.e., in order to further compose or combine a machine using this operator with other machines, the machine must be given a name in a machine definition, and can then be combined in the same manner as any other machine.

-------
## Packet-Level Packet Machines 

These machines modify the shape of a packet. If a packet does not have a suitable shape for one of these machines to perform its operation, the packet is passed to the next machine unmodified. For example, if a packet of shape [ ARP ; ETH ] is fed into the machine `extract DNS`, it will yield the packet with its original [ ARP ; ETH ] shape.



#### `pop <PROTOCOLTYPE>`

Examples:

```
pop DNS
```

`pop` accepts a protocol type (e.g. `ETH`) as an argument, and extracts the named protocol and every protocol of a *higher* OSI/TCP layer from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `pop IP4` will return a packet with the shape [ TCP ; IP4 ]

---

#### `pull <PROTOCOLTYPE>`

Examples:

```
pull DNS
```

`pull` accepts a protocol type (e.g. `ETH`) as an argument, and extracts the named protocol and every protocol on a *lower* OSI/TCP layer from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `pull IP4` will return a packet with the shape [ IP4 ; ETH ]

---

#### `extract <PROTOCOLTYPE>` 

Examples:

```
extract DNS
```

`extract` accepts a protocol type (e.g. `ETH`) as an argument, and extracts the named protocol from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `extract IP4` will return a packet with the shape [ IP4 ]

---

#### `cut <PROTOCOLTYPE>`

Examples:

```
cut DNS
```

`cut` accepts a protocol type (e.g. `ETH`) as an argument, and removes the named protocol from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `extract IP4` will return a packet with the shape [ TCP ; ETH ]

---

#### `push <PROTOCOLTYPE> (<FIELD1>=<VALUE1> <FIELD2>=<VALUE2>)`

Examples: 

```
push ARP(op=2 sha=12:34:56:78:aa:bb)
```

`push` accepts a field builder expression (a protocol type and a set of fields) and adds the message constructed by the field builder expression to the *top* of the packet. For example, if a packet has the shape [ ETH ], the example in this section will add an ARP message on top, giving a packet of shape [ ARP ; ETH ]. 

*Note*: If enumerations are used, `push` will add a message with fields of every possible value of the range of the enumerations. 

---

#### `lift <PROTOCOLTYPE> (<FIELD1>=<VALUE1> <FIELD2>=<VALUE2>)`

Examples:

```
lift ETH (src=12:34:56:78:aa:bb dst=ff:ff:ff:ff:ff:ff etherType=666)
```

`lift` accepts a field builder expression (a protocol type and a set of fields) and adds the message constructed by the field builder expression to the *bottom* of the packet. For example, if a packet has the shape [ ARP ], the example in this section will add an ETH message on bottom, giving a packet of shape [ ARP ; ETH ]. 

*Note*: If enumerations are used, `lift` will add a message with fields of every possible value of the range of the enumerations. 

---

#### `randomize <NUMBER OF PACKETS> [ <PROTOCOLTYPE1> ; <PROTOCOLTYPE2>]`

Examples:

```
randomize 10 [ARP ; ETH]
```

`randomize` generates *n* packets of the specified shape *whenever it receives a packet as input*. (If you just want to generate random packets, use the source version, `genRandoms`.) It yields the original packet and one copy of the randomized packets until it has passed along *n* randomized packets, then yields only the input packets unmodified. 

*Note*: At this time the packets are generated in a somewhat crude manner, and are not validated for protocol/port numbers, or checksum'd, the vast majority of packets generated this way will show up in wireshark (or any protocol analyzer) as corrupt or invalid. A better randomizer is on the to do list :) 

*Note*: At the moment, generating random message content messages doesn't work correctly. (It generates bytestrings that are far too long.)

---

#### `void` (No arguments)

Examples:

```
void
```

The `void` machine discards all packets that are passed to it, and never yields anything.

---

#### `checksum` (No Arguments)

Examples: 

```
checksum
```

The `checksum` machine calculates a checksum for every supported protocol message in a packet. (Note: It isn't possible to calculate an accurate checksum for each message individually, since the value of a checksum in one message will affect the value in other messages.)

Currently supports UDP, TCP, IP4, and ICMP. For TCP and UDP, if a packet is constructed that lacks an IP4 message, the checksum will not be calculated and the packet will be passed along unmodified. (The reason for this is that the calculation of Layer 4 checksums requires a "fake" header that contains information from a Layer 3 protocol.)

*Note*: I'm pretty sure the checksums are accurate. *However*, if you have checksum offloading enabled, your NIC might overwrite the checksum. If this is an issue, disable checksum offloading. (Don't think there's a universal method for doing this, you'll have to google around based on your distro/NIC.)

---

#### `count <NUMBER OF PACKETS TO COUNT>`

Examples: 

```
count 10
```

The `count` machine does not modify or filter packets, but instead counts *n* packets, reports the time that it took to process *n* packets to the terminal, and then repeats this process *ad infinitum*. 

---

#### `buffer <NUMBER OF PACKETS TO BUFFER>`

Examples: 

```
buffer 100
```

The `buffer` machine acts as a buffer; it waits until *n* packets have been passed to it, then yields them immediately down the line to the next machine.

---

## Field-Level Packet Machines


These machines either modify the record field(s) of a protocol message in a packet, or apply a boolean test to some field to determine whether to perform some action.

------

#### `set <PROTOCOLBUILDER> `

Examples:
```
set IP4(proto=77 flags.df=T src=10.10.10.10)
```

The `set` machine accepts a **Protocol Builder** and modifies the field indicated by the builder. Each `set` machine may only modify one protocol at a time, and it will modify *every* occurrence of that protocol in the packet. (In ordinary circumstances there would only be one occurrence of a protocol in each packet, but if you are doing something adventurous, keep this fact in mind.)

---

#### `alert "<STRING>" <PROTOCOLSELECTOR>`

Examples:

```
alert "someAlert" IP4(flags.df=T || checksum=0)
```

The `alert` machine accepts a quoted string and **Protocol Selector**, and prints the string to the terminal when a packet maching the selector is passed to it. 

---

#### `report "<STRING>"`

Examples: 

```
report "Got a packet!"
```

Report is like alert, but it does not require a Protocol Selector and will print the argument to the terminal whenever it receives a packet.

---

#### `create wait=<TIME IN MICROSECONDS> repeat=<NUMBER OF REPEATS> <PROTOCOLBUILDER>`

Examples:

```
create wait=0 repeat=0 [ARP (op=2 tpa=192.168.0.2) ; ETH (etherType=2054)]
```

The `create` machine accepts a time argument (in microseconds - one microsecond is 1/1000000th of a second) and a repeat argument (an integer). It yields both the created and original packet down the line to the next machine every *n* microseconds, and repeats *x* number of times before stopping. After stopping, it will yield any packets passed to it unmodified.

If you use enumeration or non-contiguous fields to create many packets in the Protocol Builder, `create` will cycle through all of them *x* times, where *x* is the number of repeats. 

*Note*: The `wait=` and `repeat=` are optional, and you can omit those arguments entirely. The machine defaults to a wait time and repeat argument of 0 if the arguments are omitted. E.g. these are all valid: 

```
create repeat=0 [ARP (op=2 tpa=192.168.0.2) ; ETH (etherType=2054)]

create [ARP (op=2 tpa=192.168.0.2) ; ETH (etherType=2054)]
```

---

#### `select <PROTOCOL> <FIELD SELECTOR>`

Examples:

```
select ARP

select ARP (op>2 && hrd!=6)
```

The `select` machine passes on any packet that satisfies the Protocol Selector expression, and discards any that do not. The fields are optional; as in the first example, you can omit the field values to select any packet that contains the chosen protocol.

---


#### `discard <PROTOCOLSELECTOR1>`

Examples:

```
discard ARP

discard TCP(win<600)
```

The `discard` machine discards any packet that satisfies the Protocol Selector expression, and passes along any that do not. The fields are optional; as in the first example, you can omit the field values to discard any packet that contains the chosen protocol.

---

## Optional Field Machines 

An *optional field* is a field that is either absent or has >= 1 inhabitants. Examples are: The IPv4 "Options" field, the DNS Question / Answer / Additional  fields, or the TCP options field. There isn't an elegant way to perform operations on these fields with normal record selector/builder syntax, so I've included a few special machines for performing these operations. 

*Note*: The one exception to the above statement is that you can use `select` and `discard` on optional fields, but `select` will pass along *any* packet that contains *at least one* member of the optional field with the chosen value (and `discard` will discard any packet with at least one field that matches the selector.)

---

#### `modifyOpt <PROTOCOL> <OPTIONAL FIELD NAME> [(<FIELDSELECTOR1>) => <FIELDBUILDER>]`

An example will probably be illuminating: 

```
modifyOpt IP4 opts [(opType.opClass/=0) => opType.opClass=8]
```

In the above example, the machine looks for an packet containing an IP4 message. When it finds one, it checks to see if any of the inhabitants of the options field have an `opType.opClass` field not equal to 0. If it finds a match, it changes the value of the `opType.opClass` field to 8.

*Note*: You can use `mOpt` instead of `modifyOpt` if you like. The following is valid: 

```
mOpt IP4 opts [(opType.opClass/=0) => opType.opClass=8]
```

---

#### `insertOpt <PROTOCOL> <OPTIONAL FIELD NAME> <FIELDBUILDER>`

Again, I think an example is probably more useful than a deep explanation of the syntax:

```
insertOpt IP4 opts (opType.opNum=2 opType.opClass=3 opLength=6 opData=face)
```

In the above example, the machine looks for a packet containing an IP4 message. When it finds one, it inserts an IP4 option with an opNum of 2, an opClass of 3, an opLength of 6, and opData "face" ("face" here is a hexadecimal bytestring).

*Note*: You can use `iOpt` instead of `insertOpt` if you like. The following is valid: 

```
iOpt IP4 opts (opType.opNum=2 opType.opClass=3 opLength=6 opData=face)
```

---

#### `deleteOpt <PROTOCOL> <NAME OF OPTIONAL FIELD> <FIELDSELECTOR>`

Example: 

```
deleteOpt IP4 opts (opType.opNum=3 || opType.opNum<2)
```

In the above example, the machine deletes any options with an opNum of 3 or an opNum <2 from any packet it finds that contains such a field.

*Note*: As in the above example, you can use `dOpt` instead of `deleteOpt` if you like.


---

## Effectful (IO) Machine

The machines in this group all perform some action (pretty printing packets to the terminal, writing them to a file, etc) instead of or in addition to modifying packets in some way. (This might seem like an odd category unless you are a Haskell programmer...)

---

#### `prettyPrint <PRINTMODE>`

Examples:

```
prettyPrint default

prettyPrint hex

prettyPrint bin

prettyPrint

pp
```

Pretty prints a representation of each packet it receives to the terminal, then yields that packet down the line. The three print modes are `default`, `hex`, and `bin`. Default is generally base 10, except for types with a special format (IP4 or MAC addresses, DNS names, etc).

The mode argument is optional. If it is not present, the machine will print everything in default. 

*Note* You may use `pp` instead of the long name `prettyPrint`.

*Note* If you intend to process a *lot* of packets, don't use this. I wrote the prettyPrinter myself instead of using a library, and it does OK at reasonable loads, but it is probably one of the least efficient parts of the program overall and will hog resources if overloaded.

--- 

#### `writeField path="<PATH OF FILE TO WRITE>" label="<STRING>" mode=<PRINTMODE> <PROTOCOL> <FIELD NAME>`

Examples: 

```
writeField path="/home/someUser/IP4s.txt" label="label" mode=hex IP4 src

writeField path="/home/someUser/IP4s.txt" hex IP4 src

wf path="/home/someUser/IP4s.txt" IP4 src
```

Writes a textual representation of the selected field to a file. The printmode argument determines whether it will be binary, hex, or whatever the default formatting option is. The label argument is a prefix to the field on each line of the file. E.g. label="MaybeLeetHackers" will create lines prefixed with "MaybeLeetHackers: ". 

The `path` argument is mandatory. Label defaults to nothing, printmode defaults to default if not supplied. Only one field can be selected at a time. Appends by default.

Can use `wf` instead of the full `writeField`.


---

#### `dump path="<PATH TO DUMP FILE>" <MAX NUMBER OF PACKETS TO DUMP>` 

Examples: 

```
dump path="/home/gnumonic/DUMPPPP" numPackets=10000"
```

Writes all packets it receives to a PCAP file indicated by the `path="x"` argument. Uses the "standard" PCAP format (not the next generation format). Will reject any packet that is larger than 65535 bytes in size. 

*Note*: I'm not using libPcap's built in function (was easier to write it myself than to figure out how to write a storable instance for sum types), and it definitely produces readable pcap files, but I can't promise there isn't some edge case that I didn't handle correctly. 

---

#### `send` (No arguments)

Example: 

```
send
```

Sends a packet, then yields that packet to the next machine in the chain.

--- 

## Higher Order Machines

The machines in this section take one or more other machines as an argument. They allow for basic control flow or branching operations over the streams of packets that they are fed. 

The `<MACHINE>` arguments to these packet workers doesn't have to be a single machine, it can be a chain of them composed with `~>`, or a named machine defined by you. All sequences of machines passed as arguments to these machines **must be enclosed within parentheses**.

---

#### `limit <NUMBER OF TIMES TO RUN MACHINE> (<MACHINE>)

Examples: 

```
limit 100 (report "Yay!")

limit 100 (select ARP (op/= 1 && op /= 2) ~> report "Got a weird arp!")
```

Runs the argument machine *n* times, then yields no further packets. 

*IMPORTANT NOTE*: Limit remembers the constraint number for the *entire lifetime of the machine*. If you *stop* a factory that contains a limit machine, then start it again, the count does *not reset*.


---

#### `switch <SWITCHMODE> <PROTOCOL> <FIELDSELECTOR> (<MACHINE1>) (<MACHINE2>)`

Examples: 

```
switch reset IP4(flags.df=T) (void) (pp)

switch IP4(flags.df=T) (void) (pp)

switch blow IP4(flags.df=T) (void) (pp)

sw IP4(flags.df=T) (void) (discard ICMP ~> pp)
```

The `switch` machine operates as a switch. It runs the first machine until it receives a packet that satisfies the Field Selector for its protocol argument, then runs the second machine. If the switch is activated in `reset` mode, it will change back to the first machine from the second machine if it receives a(nother) packet that satisfies the selector. The mode argument can be omitted; the machine defaults to `blow` mode. 

`sw` can be used instead of `switch

---

#### `countSwitch <NUMBER TO COUNT> <SWITCHMODE> (<MACHINE1>) (<MACHINE2>)`

Examples: 

```
countSwitch 100 reset (pp) (void)

swC 100 reset (pp) (void)
```

Works like `switch` except it alternates machines based on the number of packets it has processed instead of a selector. If initialized in `reset` mode, it will alternate back to the first machine after passing *n* packets to the second machine.

---

#### `timeSwitch <TIMER IN MICROSECONDS> (<MACHINE1>) (<MACHINE2>)`

Examples: 

```
timeSwitch 500000 (pp) (void)

swT 500000 (pp) (void)
```

Works like `switch` or `countSwitch` with the exception that it doesn't accept a mode argument (for now; I'm working on it!) and therefore never resets. Note that there are 1,000,000 microseconds in a second. 

*Note*: Because of the way Haskell's runtime works, the time argument is the *minimum*, i.e., timeSwitch will wait *at least* the indicated number of microseconds before switching to the second argument machine. (It should be close enough for most purposes, but Haskell isn't good at realtime tasks, and the garbage collector might throw it off by a few microseconds.)

---

#### `until <PROTOCOL> <FIELD SELECTOR> (<MACHINE>)`

Examples: 

```
until TCP(seqNum>100) (pp)
```

`until` runs the argument machine until it received a packet that satisfies the selector expression, then stops yielding packets entirely. 

---

#### `after <PROTOCOL> <FIELD SELECTOR> (<MACHINE>)`

Examples: 

```
after TCP(seqNum>100) (pp)
```

The inverse of `until`. Discards all packets until it is passed one that satisfies the selector expression, then runs the argument machine on all inputs forever. 

--- 

#### `when <PROTOCOL> <FIELD SELECTOR> (<MACHINE>)`

Examples: 

```
when IP4(flags.df=1) (void)
```

`when` checks whether its input packets satisfy the selector expression and runs the argument machine on it (+ yields the output of running that machine) if the packet does satisfy the selector. If the packet does not satisfy the selector, it is yielded down the chain unmodified. 

---

#### `unless <PROTOCOL> <FIELD SELECTOR> (<MACHINE>)`

Examples: 

```
unless IP4(flags.df=1) (void)
```

`unless` is the inverse of `when`. It yields any packets unmodified that do satisfy the selector expression down the chain, and yields the product of running the argument machine on its input down the line if the input does not satisfy the selector.

---

#### `case [(<PROTOCOL> <FIELD SELECTOR>) => (MACHINE1) ; (<PROTOCOL> <FIELD SELECTOR>) => (MACHINE2) ; (...) ]`

Examples: 

```
case [IP4 => (pp) ; ARP(op>2 || hrd/=4) => (report "Yay!") ]
```

`case` takes an arbitrary number of selector expressions and machines, checks each packet against each selector, and yields the result of feeding the packet into the machine associated with the *first* matching selector. If the packet does not match any selector, it is discarded.

--- 

#### `listenFor (See explanation for the arguments)` 

Example:

```
listenFor [ * ; IP4 (proto=$(proto)) ; * ] timeout=2.5 maxTimeouts=3 multiplier=.4 onResponse=(void)

listenFor [ * ; ARP (op<3) ; * ] timeout=2.5 maxTimeouts=3 multiplier=.4 onResponse=(void)

listenFor [ * ; IP4(src=$(dst) || dst=$(src)) ; * ] timeout=2.5 maxTimeouts=3 multiplier=.4 onResponse=(void)
```

`listenFor` is probably the most complicated machine, so it needs a bit of an explanation. 

The first argument to `listenFor` is a 'list' that represents the shape of a packet. `*` is a wildcard; in the first example above, `listenFor` will match on any packet containing an IP4 message *anywhere* that satisfies the selector (more on that weird selector in a moment).

You can use normal selector expressions in `listenFor`, but, uniquely, `listenFor` also allows you to **reference** fields in the packets that it receives as input. The `IP4 (proto=$(proto))` part of the first example above means: "Listen for any packet with a protocol number that matches the protocol number of the packet `listenFor` was passed." In the third example, `IP4(src=$(dst) || dst=$(src))` means: "Listen for any packet with a source equal to the input packet's destination, or a destination equal to the input packet's source."

The second argument is the `timeout` for the listen request, in seconds. In all of the examples, Pλacket will wait for 2.5 seconds before marking a timeout. 

The third argument is the *maximum number of timeouts* allowed before Pλacket will stop listening for a response. 

The fourth argument is the timeout *multiplier*. Each time that a timeout occurs, Pλacket modifies the previous timeout value by the multiplier. In each of the above examples, the Pλacket will wait 2.5 seconds before marking the first timeout, then 1 second (2.5 * .4 = 1) before marking the second timeout, then, finally, .4 seconds before abandoning the listen request. 

The fifth argument `onResponse=(<MACHINE>)` indicates the packet to be run on any packet that satisfies the listen request. 

The thought behind `listenFor` was that it could be used to write probes, though there are probably a bunch of other applications for it!

---

## Sources 

Now that you know how the machines work, you need to connect them to some input! Fortunately, there are fewer sources than machines, so this section should be a little easier to digest. Here are the sources: 

---

#### `generate wait=<DELAY IN MICROSECONDS> repeat=<NUMBER OF REPEATS> [<PROTOCOL> <FIELDBUILDER> ; <PROTOCOL2> <FIELDBUILDER2> (..etc..)]`

Examples: 

```
generate wait=10 repeat=10 [ARP (op=7) ; ETH (etherType=2054)]

generate 10 10 [ARP (op=7) ; ETH (etherType=2054)]
```

Generate works exactly the same way that `create` does, except generate is a source. (The difference is: `create` only creates packets when it receives some packet as input, whereas `generate` will continuously yield packets according to the arguments until it has exhausted the repeats).


--- 

#### `genRandoms num=<NUM TO GEN> wait=<DELAY IN MICROSECONDS> repeat=<NUM REPEATS> [<PROTOCOL> ; PROTOCOL ; (...etc.)]`

Examples: 

```
genRandoms num=100 wait=500 repeat=1000 [DNS ; UDP ; IP4 ; ETH]

genRandoms 100 500 1000 [DNS ; UDP ; IP4 ; ETH]

genRandoms  [DNS ; UDP ; IP4 ; ETH]
```

`genRandoms` is the source version of `randomize`, and functions exactly the same way with the exception that it will yield packets continuously until it has exhausted its repeats. 

---

#### `readPcap path="<PATH TO PCAP FILE>"`

Examples: 

```
read path="/path/to/pcap.pcap"
```

`readPcap` reads a pcap file and yields all packets from that file that Pλacket is able to deserialize. (Uses libPcap so should work with the next generation pcap format).


---

#### `listen` (No arguments)

Examples:

```
listen
```

`listen` listens for packets on a device. At the moment (...mainly because I only have one NIC that works with Linux...), the device is always the default device selected by the Network.Pcap library. Fixing this to allow users to select their device for listening and sending is a high priority once I get a new NIC.

---


#### `(<SOURCE1>) :Y: (<SOURCE2>)`

Examples: 

```
(listen) :Y: (genRandoms  [DNS ; UDP ; IP4 ; ETH])
```

`:Y:` is a *source combinator*. The `:Y:` combinator accepts two sources enclosed in parentheses and reads from each non-deterministically, i.e., it does not strictly alternate between each source or evenly interleave one with the other, but reads from each as soon as it is available. `:Y:` can be nested, such that, e.g: 

```
((SOURCE1) :Y: (SOURCE2)) :Y: (SOURCE3)
``` 

Should work.

The parentheses are not optional, though they might become optional once I have some time to rework the parser. 


--- 


#### `(<SOURCE1>) :T: (<SOURCE2>)`

Examples: 

```
(listen) :T: (genRandoms  [DNS ; UDP ; IP4 ; ETH])
```

`:T:` is a *source combinator*. The `:T:` combinator accepts two sources enclosed in parentheses and reads from each -deterministically, i.e., it does  strictly alternate between each source and evenly interleaves one with the other. If *one* source becomes unavailable, the composite source creates with `:T:` will stop yielding.  `:T:` can be nested, such that, e.g: 

`((SOURCE1) :T: (SOURCE2)) :T: (SOURCE3)` 

Should work.

The parentheses are not optional, though they might become optional once I have some time to rework the parser. 

---

# Commands

By now, you know how machines and sources work. Here, I'm going to explain how you can *combine* and *run* those machines, and go over the various commands the program supports. 

Terminological note: A **factory** is a **source** that is connected to a **machine**.

Probably the most common command you will use is `run`. Its syntax is:

```
run <SOURCE> >> <MACHINE> ~> <MACHINE2> ~> (..etc..)
```

`run` lets you activate a factory (a source and a machine that accepts input from that source) *without having to first define them*. 

If, for instance, you wanted to feed the output of `listen` into `select ARP ~> limit 10 ( pp)` (A machine which will print the first 10 ARP messages it receives), you would do that with: 

```
run listen >> select ARP ~> limit 10 ( pp)`
```
Upon doing so, you should see some output like: 


```
Successfully activated Factory 114:
listen >> select ARP ~> limit 10 ( pp)

|---------------------------<<<  EthernetFrame  >>>----------------------------|
|     dst: e4:a7:a0:7:5e:72     src: 0:d8:61:a9:30:69     etherType: 2054      |
|------------------------------------------------------------------------------|
|-----------------------------<<<  ARPMessage  >>>-----------------------------|
|   hrd: 1   pro: 2048   hln: 6   pln: 4   op: 1   sha: 0:d8:61:a9:30:69       |
|       spa: 192.168.0.150       tha: 0:0:0:0:0:0       tpa: 192.168.0.3       |
|------------------------------------------------------------------------------|
|--------------------------<<<  Message Content  >>>---------------------------|
|                                      ""                                      |
|------------------------------------------------------------------------------|
(...)
```
The line: 

```
Successfully activated Factory 114
```

Contains the **Factory Name**, in this case, **144**. Since we ran this factory without first defining it, but might want to stop it from running at some point, the **Factory Name** gives us a way to refer to the machine. 

Instead of using run, you could **define** the factory to give it a name: 

```
f: print10ARP = listen >> select ARP ~> limit 10 ( pp)
```

Prefixing a line with `f:` is how we let Pλacket know that we are defining a factory, that is, a matched source and (chain of) machine(s).

We could then start this factory with the `start` command instead of using run. (You can also use the start command to restart an anonymous packet factory that has been paused.)

```
start print10ARP

Successfully activated Factory

Factory Name: print10ARP

listen >> select ARP ~> limit 10 (pp)
```

We can stop a packet factory, whether it has been initialized with the `start` or `run`, using the `stop` command, e.g.

```
stop 114

Successfully stopped factory 114
```

```
stop print10ARP

Successfully stopped factory print10ARP
```

The `stop` command pauses a packet factory, but the factory maintains its state. In the above examples, the `limit` machine keeps track of the number of packets passed to it over its entire lifetime. Consequently, if we were to restart `print10ARP`, and it had already printed 10 packets prior to pausing it, it will *not* reset. 

In order to reset the state of a packet factory, we must use the `kill` command. E.g.

```
kill print10ARP

Successfully killed factory print10ARP
```

If, after running the kill command, we restarted the factory with `start print10ARP`, the limit counter would reset, and it would once again prettyPrint 10 ARP packets to the terminal. 

We can view all factories (either those that are explicitly defined by us or run anonymously using `run`) by using the `showFactories` command: 

```
showFactories 

|--------------------------<<<  Name: print10ARP  >>>--------------------------|
|Factory ID: ec9e                                                              |
|- - - - - - - - - - - - - - <<<  Source code:  >>>- - - - - - - - - - - - - - |
|print10ARP = listen  >> select ARP ~> limit 10 ( pp)                          |
|-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - |
|Status: Inactive                                                              |
|------------------------------------------------------------------------------|
|------------------------------------------------------------------------------|
```

Here, we can see whether the factory is active or not, and view its source code and name. (The factory ID is for internal use / debugging purposes and can be ignored.)

We can also view the machines and sources used to construct a factory with 

```
showMachines
```

and 

```
showSources
```

If we have no more use for a packet factory, we can delete it with `deleteFactory`. E.g.

```
deleteFactory print10ARP

Successfully killed factory print10ARP
```

Deleting a factory kills it (i.e. cancels the thread it is running on) and removes it from the store of definitions. 

```
deleteSource <SOURCE NAME>
```

and 

```
deleteMachine <MACHINE NAME>
```

Remove source and machine definitions respectively. 

The commands

```
clearFactories

clearSources

clearMachines
```

Kill and delete **all** factories, sources, and machines, respectively. 

Pλacket provides a few utility commands that might come in handy. 

First, `showArpCache` displays a table of MAC to IP Address mappings. (Note that Pλacket only records ARP requests that it has received, it does not query the system level arp cache). E.g.

```
showArpCache

|-----------------------------<<<  ARP Table  >>>------------------------------|
|IP: 192.168.0.2    MAC: b0:95:75:46:a5:cf   Time seen: 20:12:11:03:12:15      |
|------------------------------------------------------------------------------|
|IP: 192.168.0.3    MAC: e4:a7:a0:7:5e:72    Time seen: 20:12:11:03:12:12      |
|------------------------------------------------------------------------------|
|IP: 192.168.0.150  MAC: 0:d8:61:a9:30:69    Time seen: 20:12:11:03:12:13      |
|------------------------------------------------------------------------------|
|IP: 192.168.0.151  MAC: f0:18:98:83:d5:61   Time seen: 20:12:11:03:12:14      |
|------------------------------------------------------------------------------|
```

The `deviceInfo` command prints a short summary of all network interfaces, and their addresses: 

```
deviceInfo

                                                                                
|- lo -------------------------------------------------------------------------|
|  |-IP4: 127.0.0.1                                                            |
|  |-IP6: 01000000000000000000000000000000                                     |
|  |-MAC: 0:0:0:0:0:0                                                          |
|------------------------------------------------------------------------------|
                                                                                
                                                                                
|- enp37s0 --------------------------------------------------------------------|
|  |-IP4: 192.168.0.150                                                        |
|  |-IP6: bd3dcd3ebf071b8800000000000080fe                                     |
|  |-MAC: 0:d8:61:a9:30:69                                                     |
|------------------------------------------------------------------------------|
                                                                                
                                                                                
|- virbr0 ---------------------------------------------------------------------|
|  |-IP4: 192.168.122.1                                                        |
|  |-IP6: 00000000000000000000000000000000                                     |
|  |-MAC: 52:54:0:5e:2:c8                                                      |
|------------------------------------------------------------------------------|
                                                                                
                                                                                
|- virbr0-nic -----------------------------------------------------------------|
|  |-IP4: 0.0.0.0                                                              |
|  |-IP6: 00000000000000000000000000000000                                     |
|  |-MAC: 52:54:0:5e:2:c8                                                      |
|------------------------------------------------------------------------------|
```

And basic statistics for each factory can be viewed with the `showStats` command: 

```
showStats

|------------------------------------------------------------------------------|
|Name: print10ARP ID#: 54138     Packets in: 515     Packets out: 10           |
|------------------------------------------------------------------------------|
```

Finally, the `save` and `load` commands allow you to save the active machine, source, and factory definitions to a file, or load them from a file (respectively). 

`save` can be called without any arguments except the path, e.g.

```
save "/home/user/definitions.txt"

```

(The quotation marks are mandatory.) 

Or it can be called with a `mode` option. `mode=write` will overwrite the existing content of the file, while `mode=append`, which is the default when `save` is invoked without any options, adds definitions to the end. 

Load doesn't have any options, and simply tries to read definitions from the file, e.g.

```
load "/home/user/definitions.txt"
```

**Note**: At the moment, the implementation of `save` and `load` is somewhat crude. `load` will detect duplicate definitions while loading, but save does *not* detect whether a definition with the same name already exists in the file.



----

# Protocol Field Reference 

## Ethernet: ETH

#### Fields: 

- **src**        - MAC Address (e.g. "ff:ff:ef:12:34:56")

- **dst**        - MAC Address (e.g. "ff:ff:ef:12:34:56")

- **etherType**  - Word16 (0-65535)

## Address Resolution Protocol: "ARP"

### Fields: 

- **hrd**

- **pro**

- **hln**

- **pln**

- **op**

- **sha**

- **spa**

- **tha**

- **tpa**


## Internet Protocol v4: IP4

**Note** : The [] before a field indicates that it is an *optional* field, and can have zero, one, or many entries. 

### Fields: 

- **vers**

- **ihl**

- **tl**

- **id**

- **flags**

- **off**

- **ttl** 

- **protocol**

- **checksum**

- **src**

- **dst**

- [] **opts** 
  - **opType**
    - **cFlag** 
    - **opClass**
    - **opNum**
  - **opLength**
  - **opData**


## Internet Control Message Protocol (v4) : ICMP

### Fields: 

**Note**: The "|" before a field indicates that it is a *variant*, as are all other fields of the same indentation in the list. This means that you can choose any one of these fields, but only one. For example, an ICMP message can have a Destination Unreachable or Source Quench (etc.) data payload, but not both at the same time. 

- **hdr**
  - **type** 
  - **code**
  - **checksum**
- **data**

  - | **du**   - Destination Unreachable
    - **unused**    - Word32
    - **dgPortion** - ByteString

  - | **sq**   - Source Quench
    - **unused**    - Word32
    - **dgPortion** - ByteString

  - | **te**   - Time Exceeded
    - **unused**    - Word32
    - **dgPortion** - ByteString

  - | **rd**   - Redirect
    - **addr**       - IP4Address
    - **dgPortion**  - ByteString

  - | **pp**   - Parameter Problem
    -  **ptr** - Word8
    -  **unused** - "Word24" (3 bytes)
    -  **dgPortion** - ByteString

  - | **erq**  - Echo Request
    - **id** - Word16
    - **seqNum** - Word16
    - **data** - Word16 (Shouldn't this be a byteString?)

  - | **erp**  - Echo Reply
    - **id** - Word16
    - **seqNum** - Word16
    - **data** - Word16 (Shouldn't this be a byteString?)

  - | **tsrq** - TimeStamp Request
    - **id** - Word16
    - **seqNum** - Word16
    - **org** - Word32
    - **rcv** - Word32
    - **trs** - Word32

  - | **tsrp** - TimeStamp Reply
    - **id** - Word16
    - **seqNum** - Word16
    - **org** - Word32
    - **rcv** - Word32
    - **trs** - Word32

  - | **ra**   - Router Advertisement
    - **numAddrs** - Word8
    - **addrEntrySize** - Word8
    - **lifetime** - Word16
    - **entries** - ByteString

  - | **rs**   - Router Solicitation
    - **reserved** - Word32

  - | **tr** - TraceRoute
      - **id** - Word16
      - **unused** - Word16
      - **outHopCount** - Word16
      - **retHopCount** - Word16
      - **outLinkSpd** - Word32
      - **outLinkMTU** - Word32
  
  - | **amrq** - Address Mask Request
    - **id** - Word16
    - **seqNum** - Word16
    - **mask** - Word32

  - | **amrp** - Address Mask Reply
    - **id** - Word16
    - **seqNum** - Word16
    - **mask** - Word32

**Note**: In any context where you are updating or modifying the ICMP data field, *transformer syntax* is enabled. This syntax allows you to transform the field into the specified type by appending a @TYPE to the field name. For the ICMP data field, the options are:

- **data@DU**   - Destination Unreachable

- **data@SQ**   - Source Quench

- **data@TE**   - Time Exceeded

- **data@RD**   - Redirect

- **data@PP**   - Parameter Problem

- **data@ERQ**  - Echo Request ("ping")

- **data@ERP**  - Echo Reply

- **data@TSRQ** - TimeStamp Request

- **data@TSRP** - TimeStamp Reply

- **data@RA**   - Router Advertisement

- **data@RS**   - Router Solicitation

- **data@AMRQ** - Address Mask Request

- **data@AMRP** - Address Mask Reply

Note also that you must still use the correct selector expression for the chosen ICMP Data type. E.g. **data@DU.du.unused is correct**, but **data@DU.unused** will throw an error message.

## Transmission Control Protocol: TCP

### Fields: 

- **src**
- **dst** 
- **seqNum**
- **ackNum**
- **offset** 
- **flags**
  - **urg** 
  - **ack**
  - **psh**
  - **rst**
  - **syn**
  - **fin**
- **win**
- **checksum**
- **urgPtr** 
- [] **opts**
  - **opKind** - Word8
  - **opLen**  - Word8
  - **opData** - ByteString 

## User Datagram Protocol: UDP

### Fields: 

- **src** - Word16
- **dst** - Word16
- **len** - Word16
- **checksum** - Word16

## Domain Name System: DNS

### Fields:

- **hdr** - DNS Header
  - **id** - Word16
  - **qr** - Flag
  - **op** - Word8
  - **aa** - Flag 
  - **tc** - Flag 
  - **rd** - Flag 
  - **ra** - Flag
  - **z** - Word8
  - **rCode** - Word8 
  - **qdCount** - Word16
  - **anCount** - Word16
  - **nsCount** - Word16
  - **arCount** - Word16
- [] **question** - DNS Question
  - **name** - DNS Name 
  - **type** - Word16
  - **class** - Word16
- [] **answer** - DNS Answer (DNS RR)
  - **name** - DNS Name
  - **type** - Word16
  - **class** - Word16 
  - **ttl** - Word16 
  - **len** - Word16
  - **data** - DNSRR
    - | **a** - IP4Address
    - | **ns** - DNS Name
    - | **cname** - DNS Name
    - | **soa** - DNS Start of Authority
      - **mName** - DNS Name 
      - **rName** - DNS Name 
      - **serial** - Word32
      - **refresh** - Word32
      - **retry** - Word32
      - **exp** - Word32
      - **min** - Word32
    - | **ptr** - DNS Name 
    - | **mx** - DNS MX record
      - **pref** - Word16
      - **exch** - DNS Name 
    - | **txt** - ByteString
- [] **auth** - DNS Auth. (DNS RR)
  - **name** - DNS Name
  - **type** - Word16
  - **class** - Word16 
  - **ttl** - Word16 
  - **len** - Word16
  - **data** - DNSRR
    - | **a** - IP4Address
    - | **ns** - DNS Name
    - | **cname** - DNS Name
    - | **soa** - DNS Start of Authority
      - **mName** - DNS Name 
      - **rName** - DNS Name 
      - **serial** - Word32
      - **refresh** - Word32
      - **retry** - Word32
      - **exp** - Word32
      - **min** - Word32
    - | **ptr** - DNS Name 
    - | **mx** - DNS MX record
      - **pref** - Word16
      - **exch** - DNS Name 
    - | **txt** - ByteString
- [] **add** - DNS Add. (DNS RR)
  - **name** - DNS Name
  - **type** - Word16
  - **class** - Word16 
  - **ttl** - Word16 
  - **len** - Word16
  - **data** - DNSRR
    - | **a** - IP4Address
    - | **ns** - DNS Name
    - | **cname** - DNS Name
    - | **soa** - DNS Start of Authority
      - **mName** - DNS Name 
      - **rName** - DNS Name 
      - **serial** - Word32
      - **refresh** - Word32
      - **retry** - Word32
      - **exp** - Word32
      - **min** - Word32
    - | **ptr** - DNS Name 
    - | **mx** - DNS MX record
      - **pref** - Word16
      - **exch** - DNS Name 
    - | **txt** - ByteString

**Note**: In any context where you are updating or modifying the DNSRR field, *transformer syntax* is enabled. This syntax allows you to transform the field into the specified type by appending a @TYPE to the field name. For the DNSRR data field, the options are:

- **data@A**     - IP4Address

- **data@NS**    - DNS Name

- **data@CName** - DNS Name

- **data@SOA**   - DNS Start of Authority

- **data@PTR**   - DNS Name

- **data@MX**    - DNS MX 

- **data@TXT**   - ByteString

