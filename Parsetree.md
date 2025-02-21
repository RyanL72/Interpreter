Program
├── StatementList
│   ├── VarDeclaration
│   │   └── Identifier: x
│   ├── Assignment
│   │   ├── Identifier: x
│   │   └── Literal: 10
│   ├── VarDeclaration
│   │   ├── Identifier: y
│   │   └── Expression
│   │         ├── Term: 3
│   │         ├── Operator: *
│   │         ├── Identifier: x
│   │         ├── Operator: +
│   │         └── Literal: 5
│   ├── WhileStatement
│   │   ├── Condition: Expression
│   │   │   ├── Left: Expression
│   │   │   │    ├── Identifier: y
│   │   │   │    ├── Operator: %
│   │   │   │    └── Identifier: x
│   │   │   ├── Operator: !=
│   │   │   └── Right: Literal: 3
│   │   └── Body: Statement
│   │         └── Assignment
│   │              ├── Identifier: y
│   │              └── Expression
│   │                    ├── Identifier: y
│   │                    ├── Operator: +
│   │                    └── Literal: 1
│   └── IfStatement
│       ├── IfBranch
│       │   ├── Condition: Expression
│       │   │   ├── Identifier: x
│       │   │   ├── Operator: >
│       │   │   └── Identifier: y
│       │   └── Then: ReturnStatement
│       │         └── Identifier: x
│       ├── ElseIfBranch
│       │   ├── Condition: Expression
│       │   │   ├── Left: Expression
│       │   │   │    ├── Identifier: x
│       │   │   │    ├── Operator: *
│       │   │   │    └── Identifier: x
│       │   │   ├── Operator: >
│       │   │   └── Identifier: y
│       │   └── Then: ReturnStatement
│       │         └── Expression
│       │               ├── Identifier: x
│       │               ├── Operator: *
│       │               └── Identifier: x
│       ├── ElseIfBranch
│       │   ├── Condition: Expression
│       │   │   ├── Left: Expression
│       │   │   │    ├── Identifier: x
│       │   │   │    ├── Operator: *
│       │   │   │    └── Expression
│       │   │   │           ├── Identifier: x
│       │   │   │           ├── Operator: +
│       │   │   │           └── Identifier: x
│       │   │   ├── Operator: >
│       │   │   └── Identifier: y
│       │   └── Then: ReturnStatement
│       │         └── Expression
│       │               ├── Identifier: x
│       │               ├── Operator: *
│       │               └── Expression
│       │                      ├── Identifier: x
│       │                      ├── Operator: +
│       │                      └── Identifier: x
│       └── ElseBranch
│             └── ReturnStatement
│                   └── Expression
│                         ├── Identifier: y
│                         ├── Operator: -
│                         └── Literal: 1
