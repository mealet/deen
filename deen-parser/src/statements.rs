use crate::{expressions::Expressions, types::Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statements {
    AssignStatement {
        identifier: String,
        value: Expressions,
        span: (usize, usize)
    },
    BinaryAssignStatement {
        identifier: String,
        operand: String,
        value: Expressions,
        span: (usize, usize)
    },
    DerefAssignStatement {
        identifier: String,
        value: Expressions,
        span: (usize, usize)
    },
    SliceAssignStatement {
        identifier: String,
        index: Expressions,
        value: Expressions,
        span: (usize, usize)
    },

    AnnotationStatement {
        identifier: String,
        datatype: Option<Type>,
        value: Option<Expressions>,
        span: (usize, usize)
    },

    FunctionDefineStatement {
        name: String,
        datatype: Type,
        arguments: Vec<(String, Type)>,
        block: Vec<Statements>,
        span: (usize, usize)
    },
    FunctionCallStatement {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize)
    },

    IfStatement {
        condition: Expressions,
        then_block: Vec<Statements>,
        else_block: Option<Vec<Statements>>,
        span: (usize, usize)
    },
    WhileStatement {
        condition: Expressions,
        block: Vec<Statements>,
        span: (usize, usize)
    },
    ForStatement {
        initializer: Box<Statements>,
        condition: Expressions,
        iterator: Box<Statements>,
        block: Vec<Statements>,
        span: (usize, usize)
    },

    ImportStatement {
        path: Expressions,
        span: (usize, usize)
    },

    BreakStatements {
        span: (usize, usize)
    },
    ReturnStatement {
        value: Expressions,
        span: (usize, usize)
    },

    Expression(Expressions),
    None
}
