# OAsm

A tiny 8-bit assembly interpreter in OCaml.

```
dune exec oasm -- program.oasm
dune test
```

```
SET R0, 0
LOOP
    INC R0
    COMPARE R0, 10
    IF_EQUAL
        BREAK
    END
END
HALT
```

**Registers** `R0`-`R7`, 8-bit, 256 bytes of memory.

**Instructions**
`SET` `COPY` `READ` `WRITE` `PASS` `HALT`
`ADD` `SUB` `MUL` `DIV` `MOD` `AND` `OR` `XOR` `NOT` `LSHIFT` `RSHIFT` `INC` `DEC`
`COMPARE` `JUMP` `CALL` `RETURN` `PUSH` `POP`
`LOOP` `BREAK` `IF_EQUAL` `IF_NOT_EQUAL` `IF_LESS` `IF_LESS_EQUAL` `IF_GREATER` `IF_GREATER_EQUAL` `END`

Labels end in `:`, comments start with `;`.
