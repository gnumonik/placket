# Pλacket

Pλacket (or placket if you hate Greek letters) is a command-line utility, written in Haskell and based on libpcap, that interprets a small reactive scripting language which generating and processing streams of raw network packets.

Pλacket allows the user to define and combine a variety of *sources* (e.g. network devices, pcap files, or packet generating functions), connect those sources to *packet machines* that modify, filter, or perform effectful actions (such as sending, writing to a pcap, pretty printing packets or fields to the terminal or a file, etc). 


In this documentation, it will sometimes be useful to refer to the *shape* of a packet. The shape of a packet is the set of protocols, represented in descending order from left to right. For example, a typical DNS packet might have the shape [DNS ; UDP ; IP4 ; ETH].

# Installation 
Pλacket *should* be compatible with any modern Linux distribution. Windows is not supported at this time. (It may be possible to build Pλacket on MacOS, but this has not been tested.)

At the moment, the only way to install Pλacket is to compile it using the stack build tool. You can find instructions for installing stack [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux)

Pλacket requires two external dependencies: libtinfo and libpcap-dev

If you do not have these libraries present, they can be installed on Ubuntu with:

`sudo apt install libtinfo` 

`sudo apt install libpcap-dev` 

(Replace `sudo apt install` with `sudo dnf install` for Fedora).

If your distribution's repository does not contain libtinfo, you might try installing the ncurses-compat-libs package, which should satisfy the dependency. 

After installing the dependencies, clone this repository with:

`git clone https://github.com/gnumonik/placket.git`

Change directories: 

`cd ./placket`

And build Pλacket with the command:

`stack build`

*Note*: Compiling Pλacket might take a while, especially if you don't have (the needed versions of) any of the Haskell dependencies required.

*Note*: Stack will download the appropriate version of the GHC Haskell compiler. The Haskell compiler is quite large, but after compiling you may delete it without any negative consequences. 

Upon successful compilation of Pλacket, you should see a line of output that looks like:

`Installing executable placket in </PATH/TO/BINARY>`

The binary will be located in the `</PATH/TO/BINARY>` folder, and you may move it to a more suitable location if you like. 

**IMPORTANT**: Because Pλacket depends on libpcap, you must either run it as root or set the CAP_NET_RAW and CAP_NET_ADMIN capabilities for your user account on your system. I **strongly** recommend that that you do not run Pλacket as root and instead take the capabilities approach. (Consult google for specific information on setting capabilities for your system.)




# Packet Machines 

User defined machines are constructed by combining and configuring the built-in machines provided by Pλacket. 

The syntax for defining a machine is: `m: <NAME> = <MACHINE> `, where `<NAME>` is the name of the machine (only alphanumeric characters and underscores are allowed), and  `<MACHINE>` is either a a built-in machine, a user machine, or a combination (or composition) of built-in and/or user-defined machines.

There are three primitive operators for combining machines:

#### (**~>**): `<MACHINE1> ~> <MACHINE2> :|`

This operator composes `<MACHINE1>` and `<MACHINE2>` to form a composite machine, which can be reused in the construction of other machines or run directly. When an expression of the form `<MACHINE1> ~> <MACHINE2> :|` is compiled, the resulting composite machine first applies `<MACHINE1>` and then feeds its input into `<MACHINE2>`.

The `~>` operator can be used to chain machines together *ad infinitum*. Any expression of the form `<MACHINE1> ~> <MACHINE2> ~> (...) ~> <MACHINEX>` will compile to a single machine. Each chain of machines must end with the `:|` operator, which simply signifies to Pλacket that it can stop parsing the chain of machines. 

#### (**~+>**): `<MACHINE1> ~+> <MACHINE2> :| <MACHINE3> :| <MACHINE4> :| (etc)` 

The `~+>` operator is the *fanout* operator. It takes the output of the machine on the left, and feeds it into each of the machines on the right. The composite machine produced by the fanout operator yields the output of each of the machines on the right.

Note that a machine definition that employs the `~+>` operator must end with the rightmost `:|` following the `~+>`. In order to further compose or combine a machine using this operator with other machines, the machine must be given a name in a machine definition, and can then be combined in the same manner as any other machine. 

The built-in machines are: 

## Packet-Level Packet Machines 

These machines modify the shape of a packet. If a packet does not have a suitable shape for one of these machines to perform its operation, the packet is passed to the next machine unmodified. For example, if a packet of shape [ ARP ; ETH ] is fed into the machine `extract DNS`, it will yield the packet with its original [ ARP ; ETH ] shape.

#### `pop <PROTOCOLTYPE>` 

`pop` accepts a protocol type (e.g. `ETH`) as an argument, and extracts the named protocol and every protocol of a *higher* OSI/TCP layer from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `pop IP4` will return a packet with the shape [ TCP ; IP4 ]

#### `pull <PROTOCOLTYPE>`

`pull` accepts a protocol type (e.g. `ETH`) as an argument, and extracts the named protocol and every protocol on a *lower* OSI/TCP layer from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `pull IP4` will return a packet with the shape [ IP4 ; ETH ]

#### `extract <PROTOCOLTYPE>` 

`extract` accepts a protocol type (e.g. `ETH`) as an argument, and extracts the named protocol from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `extract IP4` will return a packet with the shape [ IP4 ]

#### `cut <PROTOCOLTYPE>`

`cut` accepts a protocol type (e.g. `ETH`) as an argument, and removes the named protocol from the packet. For example, if a packet has the shape [TCP ; IP4 ; ETH], `extract IP4` will return a packet with the shape [ TCP ; ETH ]

## Field-Level Packet Machines

These machines either modify the record field(s) of a protocol message in a packet, or apply a boolean test to some field to determine whether to perform some action.

### Record Syntax 
Each of these machines uses a variant of Pλacket's **record syntax** to access (and possibly modify) fields in a given packet. The record syntax varies slightly with the function of the machine (e.g. only machines that filter packets based on some field have employ the boolean comparison operators `>= / <= etc`), but the basic idea is the same.

The general format for record syntax is: 

`<PROTOCOLTYPE> ( <FIELD1>.<SUBFIELD>.<SUBSUBFIELD>=<VALUE1> <FIELD2>.<SUBFIELD2>.<SUBSUBFIELD2>=<VALUE2> (etc) )` 

For machines that perform boolean tests on fields, the `=` maybe be replaced with `>=`,`<=`,`>`,`<`. `=` is overloaded; in a context of setting packet fields, it means "set this field to the value on the right", whereas in a context of performing boolean tests it means "True if the field is equal to the value on the right".

In this documentation, a context that uses record syntax to modify or construct fields will be referred to as a **Record Builder**, while a context that uses record syntax to perform boolean tests will be known as a **Record Selector**. The sub-expression `<FIELD1>.<SUBFIELD>=<VALUE>` will be referred to as a **Field Builder** or **Field Selector** depending upon the context. 

**Record Builders** 

The parentheses in record syntax are not optional, but the fields in between them may be omitted. In the **Record Builder** context, a lack of fields between the parentheses will construct a protocol message with *all default values*. Because Pλacket aims to give maximum control over the construction of raw packets, the default values are **not** "sensibly chosen". For all numerical fields, the default value is 0. For all flags, the default value is False or Off. For all ByteString fields, the default value is the empty bytestring. A protocol message that contains optional fields (fields that might have zero or more members) constructed with the default values will be set with 0 members. 

Furthermore, in the **Record Builder** context only, all numerical (or number-like fields) can be *enumerated* with a dash between the start value and the end value. The only field types that cannot be enumerated are Flags and ByteStrings. For example, the expression:

`IP4 (src=0.0.0.0-0.0.0.10)`

will construct 11 IPv4 messages. The first will have a source IP address of 0.0.0.0, while the last will have a source IP address of 0.0.0.10. **Warning:** use of multiple enumerations at the same time can cause the number of packets generated to grow exponentially. Though I have done my best to exploit Haskell's laziness to make generating large numbers of packets efficient, certain machines (particularly those that perform IO, like `send` or `dump`) will consume large amounts of memory if extremely high numbers of  packets are generated. 

In addition to enumeration, one can construct a non-continguous set of values with a record builder by enclosing multiple values between `<` and `>`. The expression:

`IP4 (src=<192.168.2.30 192.168.2.50>)` 

constructs two IP4 protocol messages, one with a source of 192.168.2.30 and another with a source of 192.168.2.50

**Record Selectors** 

As mentioned above, basic **Record Selector** syntax is nearly identical to the **Record Builder** syntax, differing only in  that `=` means "True if the field in the packet passed to the machine matches the value on the right", that the operators `>= <= > <` can replace `=`, and that enumerations and non-contiguous sets are not allowed.

More advanced record selectors can be constructed by combining Record Selectors and their component Field Selectors using boolean operators using a notation that is very similar to the BPF syntax to create compound truth functional expressions. Both Field Selectors (within the parentheses) and Record Selectors (the whole expression containing the Protocol Type and its associated Field Selectors) can be combined using the operators `||` ('or'), `&&` ('and'), `not` ('not'). 

Some examples:

The expression: 

`ARP (op=1 || op=2)` will select any packet containing an ARP message with an opcode of 1 or 2. 

Similarly, the expression:

`ARP (sha=ff:ff:ff:ff:ff:ff && (op=1 || op=2))` selects any packet containing an ARP message with a broadcast sender address and either an opcode of 1 or 2. 

**Field Value Format** 

All numerical fields can be entered either as decimal numbers, binary numbers, or hexadecimal numbers. By default, Pλacket will attempt to read the value as a decimal. To enter a number in binary or hex, simply prefix the number with `0b` for binary or `0x` for hex. For example, Each of the following expressions will set the etherType field of the ethernet message in a packet to 666.

`set ETH (etherType=666)`

`set ETH (etherType=0b1010011010)`

`set ETH (etherType=0x029A)`

Fields of the ByteString type can either be entered using an ascii string enclosed withon quotation marks, or in hex (the 0x prefix is not necessary here).

**Machines** 

The machines that use this syntax are:

#### `set <PROTOCOLBUILDER> `

The `set` machine accepts a **Protocol Builder** and modifies the field indicated by the builder. Each `set` machine may only modify one protocol at a time, and it will modify *every* occurrence of that protocol in the packet. (In ordinary circumstances there would only be one occurrence of a protocol in each packet, but if you are doing something adventurous, keep this fact in mind.)

#### `alert "<STRING>" <PROTOCOLSELECTOR>`

The `alert` machine accepts a quoted string and **Protocol Selector**, and prints the string to the terminal when a packet maching the selector is passed to it. 

#### `create wait=<TIME IN MICROSECONDS> repeat=<NUMBER OF REPEATS> <PROTOCOLBUILDER>`

The `create` machine accepts a time argument (in microseconds - one microsecond is 1/1000000th of a second) and a repeat argument (an integer). It yields one packet down the line to the next machine every *n* microseconds, and repeats *x* number of times before stopping.

If you use enumeration or non-contiguous fields to create many packets in the Protocol Builder, `create` will cycle through all of them *x* times, where *x* is the number of repeats. 

#### `select [<PROTOCOLSELECTOR_1> ; <PROTOCOLSELECTOR_2>]`



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

