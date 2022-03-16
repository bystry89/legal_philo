library(DiagrammeR)



flowchart <- mermaid("
        graph LR
        A-->J
        B-->J
        J-->C
        C-->D
        D-->E
        E-->F
        F-->G
        G-->H
        H-->I
        A[All items published in 7 <br> specialist journals]
        B[All items classified <br> in the philpapers <br>category philosophy of law]
        C((5,174))
        D[Add texts <br>cited >7 times]
        E((5,953))
        F[Add texts <br>citing >4 items]
        G((9,429))
        H[Keep items <br>co-cited >5 times <br>with >1 other items]
        I((2,262))
        J[Cited >1 time]
      ")
