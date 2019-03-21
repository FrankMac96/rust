//********************************************************
// David Kerka, Frank MacDonald
// Cs 4100
// Implmenting VM with a Garbage Collector and Concurrent Threads
//******************************************************** 
extern crate byteorder;
use byteorder::{ByteOrder, BigEndian};
use std::env;
use std::fs;
use std::slice::Iter;
type Address  = usize;
//impl<R: Reader> Reader for IoResult<R>
#[derive(Debug)]
pub struct State {
	pub halt: 	bool,
	pub pc:   	u32,
	pub fp:   	u32,
	pub stack:	Vec<Val>,
	pub heap:	Vec<Val>,
	pub program:Vec<Instr>,
}
#[derive(Debug, Clone, Copy)]
pub enum Val{
	Vunit,
	Vi32(i32),
	Vbool(bool),
	Vloc(u32),
	Vundef,
	Vsize(i32),
	Vaddr(Address),
}
#[derive(Debug, Clone)]
pub enum Unop {
	Neg,
}
#[derive(Debug, Clone)]
pub enum Binop {
	Add,
	Mult,
	Sub,
	Div,
	Lt,
	Equal,
}
#[derive(Debug, Clone)]
pub enum Instr {
	Push(Val),
	Pop,
	Peek(u32),
	Unary(Unop),
	Binary(Binop),
	Swap,
	Alloc,
	Set,
	Get,
	Var(u32),
	Store(u32),
	SetFrame(u32),
	Call,
	Ret,
	Branch,
	Halt,
}
trait BinConvert { fn get_bin(binary: &mut Iter<u8>) -> Self; }     //Trait to binary to instructions
 
impl BinConvert for i32{
    fn get_bin(binary: &mut Iter<u8>) -> Self{
        let mut v = Vec::new();
        v.push(*binary.next().expect("Error converting i32"));
        v.push(*binary.next().expect("Error converting i32"));
        v.push(*binary.next().expect("Error converting i32"));
        v.push(*binary.next().expect("Error converting i32")); 
        let ret = BigEndian::read_i32(&v);
        ret
    }
}
impl BinConvert for u32{
     fn get_bin(binary: &mut Iter<u8>) -> Self{
        let mut v = Vec::new();
        v.push(*binary.next().expect("Error converting i32"));
        v.push(*binary.next().expect("Error converting i32"));
        v.push(*binary.next().expect("Error converting i32"));
        v.push(*binary.next().expect("Error converting i32")); 
        let ret = BigEndian::read_u32(&v);
        ret
    }
}
impl BinConvert for Val{
    fn get_bin(binary: &mut Iter<u8>) -> Self{    
    match binary.next().unwrap(){
            0             => Val::Vunit,
            1             => Val::Vi32(<i32 as BinConvert>::get_bin(binary)),
            2             => Val::Vbool(true),
            3             => Val::Vbool(false),
            4             => Val::Vloc(<u32 as BinConvert>::get_bin(binary)),
            5             => Val::Vundef,
            _             => panic!("Error converting to Val"),
        }
    }
}
impl BinConvert for Binop{
    fn get_bin(binary: &mut Iter<u8>) -> Self{
        match binary.next().unwrap(){
            0           => Binop::Add,
            1           => Binop::Mult,
            2           => Binop::Sub,
            3           => Binop::Div,
            4           => Binop::Lt,
            5           => Binop::Equal,
            _           => panic!("Error converting to Binop"),
        }
    }
}
impl BinConvert for Unop{
    fn get_bin(binary: &mut Iter<u8>) -> Self{
        match binary.next().unwrap(){
            0           => Unop::Neg,
            _           => panic!("Error converting to Unop"),
        }
    }
}
impl BinConvert for Instr {
    fn get_bin(binary: &mut Iter<u8>) -> Self{
        match binary.next().unwrap(){
            0                   => Instr::Push(<Val as BinConvert>::get_bin(binary)),
            1                   => Instr::Pop,  
            2                   => Instr::Peek(<u32 as BinConvert>::get_bin(binary)),
            3                   => Instr::Unary(<Unop as BinConvert>::get_bin(binary)),
            4                   => Instr::Binary(<Binop as BinConvert>::get_bin(binary)),
            5                   => Instr::Swap,
            6                   => Instr::Alloc,
            7                   => Instr::Set,
            8                   => Instr::Get,
            9                   => Instr::Var(<u32 as BinConvert>::get_bin(binary)),
            10                  => Instr::Store(<u32 as BinConvert>::get_bin(binary)),
            11                  => Instr::SetFrame(<u32 as BinConvert>::get_bin(binary)),
            12                  => Instr::Call,
            13                  => Instr::Ret,
            14                  => Instr::Branch,
            15                  => Instr::Halt,
            _                   => {
                panic!("Error converting to Instr");    
            }
        }
    }
}
pub fn instr(i: &Instr,s: &mut State){
    match i {
        Instr::Push(v)        =>{
            let temp = v.clone();
            s.stack.push(temp);
        },
        Instr::Pop              => {
                s.stack.pop();
        },
        Instr::Peek(v)        =>{
                let temp = s.stack[*v as usize].clone();
                s.stack.push(temp);
        },
        Instr::Unary(_unop)      =>{
                match s.stack.pop().unwrap() {
                    Val::Vbool(b)        => {
                        match b {
                            true                => {
                                s.stack.push(Val::Vbool(false));
                            }
                            false               => {
                                s.stack.push(Val::Vbool(true));
                            }
                        }
                    }
                    _               => panic!("Error trying to convert non Bool to unary"),
                }
        },       
        Instr::Binary(_binop)    =>{ 
            match _binop{
                Binop::Add          =>{
                    let temp1 = s.stack.pop(); //v1
                    let temp2 = s.stack.pop(); //v2
                    match temp2.unwrap() {  
                        Val::Vi32(v2)       =>{
                            match temp1.unwrap(){
                                Val::Vi32(v1)   => s.stack.push(Val::Vi32(v1 + v2)),        
                                _               => panic!("Cannot add non i32 values together"),
                            } 
                        },
                        _               => panic!("Cannot add non i32 values togetger"),
                    }
                },
                Binop::Mult         =>{
                    let temp1 = s.stack.pop(); //v1
                    let temp2 = s.stack.pop(); //v2
                    match temp2.unwrap() {  
                        Val::Vi32(v2)       =>{
                            match temp1.unwrap(){
                                Val::Vi32(v1)   => s.stack.push(Val::Vi32(v1 * v2)),        
                                _               => panic!("Cannot multiply non i32 values together"),
                            } 
                        },
                        _               => panic!("Cannot multiply non i32 values together"),
                    }
                },
                Binop::Sub          =>{
                    let temp1 = s.stack.pop();
                    let temp2 = s.stack.pop();
                    match temp2.unwrap() {
                        Val::Vi32(v2)       =>{
                            match temp1.unwrap(){
                                Val::Vi32(v1)   => s.stack.push(Val::Vi32(v1 - v2)),        
                                _               => panic!("Cannot add subtract i32 values together"),
                            } 
                        },
                        _               => panic!("Cannot add non subtract values togethger"),
                    }
                },
                Binop::Div          =>{
                    let temp1 = s.stack.pop();
                    let temp2 = s.stack.pop();
                    match temp2.unwrap() {
                        Val::Vi32(v2)       =>{
                            match temp1.unwrap(){
                                Val::Vi32(v1)   => s.stack.push(Val::Vi32(v1 / v2)),        
                                _               => panic!("Cannot divide non i32 values together"),
                            } 
                        },
                        _               => panic!("Cannot divide non i32 values togetger"),
                    }
                },
                Binop::Lt           =>{
                    let temp1 = s.stack.pop();
                    let temp2 = s.stack.pop();
                    match temp2.unwrap() {
                        Val::Vi32(v2)       =>{
                            match temp1.unwrap(){
                                Val::Vi32(v1)   => {
                                    if v2 == v1{
                                        s.stack.push(Val::Vbool(false));        
                                    }else{
                                        s.stack.push(Val::Vbool(true));
                                    }
                                }            
                                _               => panic!("Cannot boolean compare non i32 values"),
                            } 
                        },
                        _               => panic!("Cannot boolean compare non i32 values"),
                    }
                },
                Binop::Equal        =>{
                    let temp1 = s.stack.pop();
                    let temp2 = s.stack.pop();
                    match temp2.unwrap() {
                        Val::Vi32(v2)       =>{
                            match temp1.unwrap(){
                                Val::Vi32(v1)   => {
                                    if v2 == v1{
                                        s.stack.push(Val::Vbool(true));        
                                    }else{
                                        s.stack.push(Val::Vbool(false));
                                    }
                                }            
                                _               => panic!("Cannot compare non i32 values"),
                            } 
                        },
                        _               => panic!("Cannot compare non i32 values"),
                    }
                }
            }
        },    
        Instr::Swap             =>{
            let temp1 = s.stack.pop();
            let temp2 = s.stack.pop();
            s.stack.push(temp1.unwrap());
            s.stack.push(temp2.unwrap());
        },
        // make another varible type "vsize" set 
        //check I dont exceed 1024 *GB collecter 
        Instr::Alloc            =>{
            let temp2 = s.stack.pop().unwrap(); //vinit
            let temp1 = s.stack.pop().unwrap(); //Vi32(size)
            s.stack.push(Val::Vaddr(s.heap.len() as usize));
            match temp1 {
                Val::Vi32(val) =>{
                    if val as usize + s.heap.len() as usize > 1024
                    {
                     panic!("This is bigger then 1024 vals which is not allowed");
                    }
                    s.heap.push(Val::Vsize(val));
                    for _x in 0..val {
                        s.heap.push(temp2);
                    }
                }
                _    => panic!("AHAHAHAHA"),
            }
        },
        Instr::Set              =>{
            let v = s.stack.pop().unwrap();
            let idx = s.stack.pop().unwrap();
            let vaddr = s.stack.pop().unwrap();
            match vaddr {
                Val::Vaddr(base) => match idx{
                    Val::Vi32(indx) =>{
                        if indx as usize >= s.heap.len() as usize {
                            panic!("OOHHHHH");
                        }
                        else{
                            s.heap[(base as usize + (1 as usize) + (indx as usize))] = v;
                        }
                    }
                    _ =>       panic!("not a Vi32"),
                },
                _ => panic!("not a Vaddr"),
            } 
        }, 
        Instr::Get              =>{
            let idx = s.stack.pop().unwrap();
            let vaddr = s.stack.pop().unwrap();
            match vaddr {
                Val::Vaddr(base) => match idx{
                    Val::Vi32(indx) =>{
                        if indx as usize >= s.heap.len() as usize {
                            panic!("OOHHHHH");
                        }
                        else{
                           s.stack.push(s.heap[(base as usize + (1 as usize) + (indx as usize))]);
                        }
                    }
                    _ =>       panic!("not a Vi32"),
                },
                _ => panic!("not a Vaddr"),
            } 

        },
        Instr::Var(v)         =>{
            if (s.fp + v) < (s.stack.len() as u32) {
                s.stack.push(s.stack[(s.fp+v) as usize].clone());
            }else{
                panic!("Index out of range!");
            }
        },
        Instr::Store(v)         =>{
             if (s.fp + v) < (s.stack.len() as u32) {
                s.stack[(s.fp+v) as usize] = s.stack.pop().unwrap();
            }else{
                panic!("Index out of range!");
            }
        },
        Instr::SetFrame(v)      =>{
            s.stack.push(Val::Vloc(s.fp));
            s.fp = (s.stack.len() as u32) - v - 1;
        },
        Instr::Call             =>{
           let temp = s.stack.pop();
           match temp.unwrap(){
               Val::Vloc(v)        =>{
                                      s.stack.push(Val::Vloc(s.pc));
                                      s.pc = v;
               }   
               _                   => panic!("Vloc is not set before the call statement"),
           }
        },     
        Instr::Ret              =>{

            let vret = s.stack.pop().unwrap();
            let caller_pc = s.stack.pop().unwrap();
            let caller_fp = s.stack.pop().unwrap();
            let _callee_pc = s.pc;
            let callee_fp = s.fp;
            match caller_pc {
                Val::Vloc(x) => s.pc = x,
                Val::Vi32(y) => s.pc = y as u32,

                _ => panic!("Not the Vals above"),
            }
            match caller_fp {
                Val::Vloc(x) => s.fp = x,
                Val::Vi32(y) => s.fp = y as u32,

                _ => panic!("Not the Vals above"),
            }
            while s.stack.len() != callee_fp as usize
            {
                s.stack.pop();
            }
            s.stack.push(vret);

            
        },    
        Instr::Branch           =>{
            let temp = s.stack.pop().unwrap();
            let ch = s.stack.pop().unwrap();
            match ch {
                Val::Vbool(v) => {
                    if v == true {
                        match temp {
                            Val::Vloc(i) => {
                                s.pc = i;
                            },
                            _          => panic!("ERROR"),
                        }
                    }
                },
                _                       => panic!("ERROR"),
            }
        },      
        Instr::Halt             =>{
            s.halt = true;
        },
        // _   => panic!("Error within the instruction conversion"),   
    }
}
pub fn exec(s: &mut State) {
    'mainloop:loop {
        if s.halt { break 'mainloop }
        let pc = s.pc;
        s.pc = pc + 1;
        if (pc as usize) >= s.program.len() {
            panic!("exec: pc out of bounds")
        }
        let i = &s.program[pc as usize].clone();
        instr(i, s);
    }
}

fn main() -> std::io::Result<()>{
    let args: Vec<String> = env::args().collect();
    let prg:  Vec<Instr>  = Vec::new();
    let stack:Vec<Val>    = Vec::new();
    let heap: Vec<Val>    = Vec::new();
    let filename = &args[1];
	let buf = fs::read(filename)?;
	let mut buf_iter = buf.iter();
    let prog_size = <u32 as BinConvert>::get_bin(buf_iter.by_ref());  
    let mut s = State {
        halt: 	 false,
	    pc:   	 0,
	    fp:   	 0,
	    stack:   stack,	 
	    heap:    heap,	 
	    program: prg, 
    };
    for _i in 0..prog_size {
        s.program.push(Instr::get_bin(buf_iter.by_ref()));
        //println!("{:#?}", s.program[_i as usize]);
    }
    exec(&mut s);
    println!("{:?}", s.stack.pop().unwrap());
    Ok(())
}