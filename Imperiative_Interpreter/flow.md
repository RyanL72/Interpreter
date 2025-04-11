```mermaid
flowchart TD
    %% ENTRY POINT
    A[interpret file] --> B[parser]
    B --> C[interpret-statement-list]

    %% STATEMENT INTERPRETATION FLOW
    C --> D[interpret-statement]
    
    %% Statement Type Dispatch
    D -->|return| D1[interpret-return] --> E1[eval-expression] --> E2[eval-operator] --> E3[eval-binary-op2]
    D -->|var| D2[interpret-declare] --> E1
    D -->|=| D3[interpret-assign] --> E1
    D -->|if| D4[interpret-if] --> E1
    D -->|while| D5[interpret-while]
    D -->|continue| D6[continue-continuation]
    D -->|break| D7[break-continuation]
    D -->|begin| D8[interpret-block]
    D -->|throw| D9[interpret-throw] --> E1
    D -->|try| D10[interpret-try]
    
    %% interpret-statement-list recursion
    D1 --> C
    D2 --> C
    D3 --> C
    D4 --> C
    D5 --> C
    D8 --> C
    D9 --> C
    D10 --> C

    %% TRY HANDLING
    D10 --> T1[make-finally-block]
    D10 --> T2[make-try-block]
    D10 --> T3[create-throw-catch-continuation]
    T3 --> C

    %% EXPR EVALUATION
    E2 -->|unary| E1
    E2 -->|binary| E3
    E3 --> E1

    %% ENVIRONMENT HELPERS
    E1 --> F1[lookup] --> F2[lookup-variable] --> F3[lookup-in-env]
    D2 --> G1[insert]
    D3 --> G2[update] --> G3[update-existing] --> G4[update-in-frame]
    
    %% CONTINUATIONS (Control Paths)
    D5 -->|loop body| D
    D8 -->|push-frame| C
    D8 -->|pop-frame| D6
    D8 -->|pop-frame| D7
    D10 -->|finally| D8

    %% ERROR PATH
    F2 --> H1[myerror]
    G2 --> H1
    F3 --> H1

    %% ENV UTILITIES
    subgraph ENVIRONMENT
        F1
        F2
        F3
        G1
        G2
        G3
        G4
    end

    subgraph EXPRESSIONS
        E1
        E2
        E3
    end

    subgraph STATEMENT_TYPES
        D1
        D2
        D3
        D4
        D5
        D6
        D7
        D8
        D9
        D10
    end
