pub struct Block(pub Vec<Stmt>);

pub enum Stmt {
    VariableDecl(VariableDecl),
    Assignment(Assignment),
    FnDecl(FnDecl),
    Break,
    Return(Option<Expr>),
    Conditional(Conditional),
    Loop(Block),
    WhileLoop(WhileLoop),
    ForLoop(Box<ForLoop>),
    Expr(Expr),
}

pub struct VariableDecl {
    name: String,
    init: Option<Expr>,
}

pub struct Assignment {
    pub lhs: Expr,
    pub rhs: Expr,
}

pub struct FnDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Block,
}

pub struct Conditional {
    pub condition: Expr,
    pub body: Block,
    pub else_block: Option<Block>,
}

pub struct WhileLoop {
    pub cond: Expr,
    pub body: Block,
}

pub struct ForLoop {
    pub init: Stmt,
    pub cond: Expr,
    pub post: Stmt,
    pub body: Block,
}

pub enum Expr {
    Literal(Literal),
    UnaryOp,
    BinaryOp,
    Call,
}

pub enum Literal {
    String(String),
    Number(f64),
    Array(Vec<Expr>),
    Object, // todo
    Boolean(bool),
    Null,
}

pub struct UnaryOp {
    pub expr: Expr,
    pub kind: UnaryOpKind,
}

pub enum UnaryOpKind {
    Not,
    Neg,
}

pub struct BinaryOp {
    pub lhs: Expr,
    pub rhs: Expr,
    pub kind: BinaryOpKind,
}

pub enum BinaryOpKind {
    And,
    Or,
    Equal,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    NotEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub enum Call {
    Function(Expr, Vec<Expr>),
    Field(Expr, Vec<Expr>),
}
