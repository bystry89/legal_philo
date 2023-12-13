library(DiagrammeR)



flowchart <- mermaid("
        graph LR
        A-->C
        C-->D
        D-->E
        E-->F
        F-->G
        G-->H
        H-->I
        I-->J
        J-->K
        K-->L
        A[All items published in 18 <br> specialist journals & <br> cited at least once]
        C((Specialist <br>Journal <br> Collection: <br>4,683))
        D[Add items <br>citing at least 5 <br>SJC items]
        E((Citing <br>Collection<br>799))
        F[Add items cited <br>at least 6 times <br>by SJC&CC items<br>with <i>ratio</i>>1.5%]
        G((Classics<br>Collection<br>778))
        H((Combined<br>Collections<br>6,260))
        I[Co-citation graph<br> with weight=6 <br>& degree>1]
        J((720 <br>nodes))
        K[Exclude <br>small <br> components]
        L((713<br>nodes))
      ")

