#!/usr/bin/env phantomjs
function filter(src) {
    src = src.replace(/多型/g, "多相型")
    src = src.replace(/多態/g, "多相")
    src = src.replace(/多形性/g, "多相性")
    src = src.replace(/多形的/g, "多相的")
    src = src.replace(/多形型/g, "多相型")
    src = src.replace(/チェック/g, "検査")
    src = src.replace(/単体型/g, "単相型")
    src = src.replace(/主型/g, "主要型")
    
    src = src.replace(/私たち/g, "我々")
    src = src.replace(/用語/g, "項")
    
    src = src.replace(/タイプ/g, "型")
    src = src.replace(/関数アプリケーション/g, "関数適用")
    src = src.replace(/統一/g, "単一化")
    
    src = src.replace(/：/g, ":")
    
    src = src.replace(/＃/g, "#")
    src = src.replace(/「/g, "`")
    src = src.replace(/」/g, "`")
    src = src.replace(/」/g, "`")
    src = src.replace(/（/g, "(")
    src = src.replace(/）/g, ")")
    
    src = src.replace(/  /g, "  ")
    src = src.replace(/ /g, "")
    
    src = src.replace(/されている。/g, "されています。")
    src = src.replace(/である。/g, "です。")
    src = src.replace(/できる。/g, "できます。")
    
    src = src.replace(/できるが/g, "できますが")
    src = src.replace(/果たす。/g, "果たします。")
    src = src.replace(/される。/g, "されます。")
    src = src.replace(/ている。/g, "ています。")
    src = src.replace(/する。/g, "します。")
    src = src.replace(/ない。/g, "ません。")
    src = src.replace(/なった。/g, "なりました。")
    src = src.replace(/思われる。/g, "思われます。")
    src = src.replace(/する。/g, "します。")
    src = src.replace(/含む。/g, "含みます。")
    src = src.replace(/(組)む。/g, "$1みます。")
    src = src.replace(/残す。/g, "残します。")
    src = src.replace(/(当)てる。/g, "$1てます。")
    src = src.replace(/置く。/g, "置きます。")
    


        src = src.replace(/([a-zA-Z_0-9'"’λσα-τ➙+]+) *([`\]\).,;、。]) *|([a-zA-Z_0-9'"’λσα-τ➙+]+\b) */g, function(m,a,b,c){
            if(c) return c+ " ";
            return a+b+" ";
        })
    src = src.replace(/^( *)(.*)$/mg,function(m,a,src){
        if (a.match(/    /)) return a+src;
        src = src.replace(/^( *)([a-zA-Z_0-9'"’λσα-τ➙+]+)|( *)(`|\[|\()( *)([a-zA-Z_0-9'"’λσα-τ➙+]+)|( +)([a-zA-Z_0-9'"’λσα-τ➙+]+)|\b([a-zA-Z_0-9'"’λσα-τ➙+]+)/g, function(m,tl,t,bl,a,b,c,br,d,e){
            if(t)return tl+t;
            if(d)return " "+d;
            if(e)return " "+e;
            return " "+a+c;
        })
        src = src.replace(/(\(|\[|\/)[  ]+/g, "$1")
        src = src.replace(/([  ]+)([)\]/.,;。、])/g, function(m,a,b){console.log(m); return b;})
        console.log(src);
        return a + src
    })
    src = src.replace(/ +$/mg,"")
    return src;
}

var fs = require("fs")
var system = require('system');
var args = system.args;

if (args.length != 2) {
	console.log('usage: dotcnv.js filename');
	phantom.exit(0)
}

console.log(args[1])
var name = fs.absolute(args[1]);
console.log(name);

try {
	var src = fs.read(name).toString()
} catch (e) {
	console.log('usage: dotcnv.js filename');
	phantom.exit(0)
}

fs.write(name+".bk",src)
fs.write(name,filter(src))
phantom.exit(0)
