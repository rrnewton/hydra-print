

[2013.04.02] {Pent up (duplicate?) resize events}

There is a weird dynamic where resize, see the deleted-window events
pop up, but then press a key (NCurses event), and I see ANOTHER batch
of resize events come through.  The second batch is of equal length.

For example, below I resized (6 events), and then pressed 'o' (6 more
events).


    │ [dbg] Deleted windows: [0x0000000000cd0f30] created [0x0000000000cd8c40,0x0000000000d2baa0] <line 0 y 28>                                  │
    │ [dbg] CURSES Key event: KeyChar 'u' <line 1 y 28>                                                                                          │
    │0 <line 2 y 28>                                                                                                                             │
    │ [dbg] Deleted windows: [0x0000000000cd8c40,0x0000000000d2baa0] created [0x0000000000cd89a0,0x0000000000cd8ae0] <line 3 y 28>               │
    │ [dbg] Deleted windows: [0x0000000000cd89a0,0x0000000000cd8ae0] created [0x0000000000d49d70,0x0000000000d2b700] <line 4 y 28>               │
    │ [dbg] Deleted windows: [0x0000000000d49d70,0x0000000000d2b700] created [0x0000000000c563a0,0x0000000000ccca90] <line 5 y 28>               │
    │ [dbg] Deleted windows: [0x0000000000c563a0,0x0000000000ccca90] created [0x0000000000c561d0,0x0000000000d499b0] <line 6 y 28>               │
    │ [dbg] Deleted windows: [0x0000000000c561d0,0x0000000000d499b0] created [0x0000000000cccbd0,0x0000000000dc32f0] <line 7 y 28>               │
    │ [dbg] Deleted windows: [0x0000000000cccbd0,0x0000000000dc32f0] created [0x0000000000d49af0,0x0000000000d49c30] <line 8 y 28>               │
    │100 <line 9 y 28>                                                                                                                           │
    │ [dbg] Deleted windows: [0x0000000000d49af0,0x0000000000d49c30] created [0x0000000000d28fd0,0x0000000000d29110] <line 10 y 28>              │
    │ [dbg] Deleted windows: [0x0000000000d28fd0,0x0000000000d29110] created [0x0000000000d8a4e0,0x0000000000d8a7f0] <line 11 y 28>              │
    │ [dbg] Deleted windows: [0x0000000000d8a4e0,0x0000000000d8a7f0] created [0x0000000000e03f20,0x0000000000e04230] <line 12 y 28>              │
    │ [dbg] Deleted windows: [0x0000000000e03f20,0x0000000000e04230] created [0x0000000000e04530,0x0000000000e04840] <line 13 y 28>              │
    │ [dbg] Deleted windows: [0x0000000000e04530,0x0000000000e04840] created [0x0000000000c558e0,0x0000000000c55bf0] <line 14 y 28>              │
    │ [dbg] Deleted windows: [0x0000000000c558e0,0x0000000000c55bf0] created [0x0000000000c55ef0,0x0000000000c56030] <line 15 y 28>              │
    │ [dbg] CURSES Key event: KeyChar 'o' <line 16 y 28>                                                                                         │
    │200 <line 17 y 28>                                  
