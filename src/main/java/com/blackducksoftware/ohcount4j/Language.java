/*
 * Copyright 2016 Black Duck Software, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j;

import java.util.ArrayList;
import java.util.List;

import com.blackducksoftware.ohcount4j.scan.ActionScriptScanner;
import com.blackducksoftware.ohcount4j.scan.AdaScanner;
import com.blackducksoftware.ohcount4j.scan.AssemblyScanner;
import com.blackducksoftware.ohcount4j.scan.AugeasScanner;
import com.blackducksoftware.ohcount4j.scan.AutoconfScanner;
import com.blackducksoftware.ohcount4j.scan.AutomakeScanner;
import com.blackducksoftware.ohcount4j.scan.AwkScanner;
import com.blackducksoftware.ohcount4j.scan.BatScanner;
import com.blackducksoftware.ohcount4j.scan.BfkScanner;
import com.blackducksoftware.ohcount4j.scan.BfkppScanner;
import com.blackducksoftware.ohcount4j.scan.BinaryScanner;
import com.blackducksoftware.ohcount4j.scan.BlitzMaxScanner;
import com.blackducksoftware.ohcount4j.scan.BooScanner;
import com.blackducksoftware.ohcount4j.scan.CMakeScanner;
import com.blackducksoftware.ohcount4j.scan.CStyleScanner;
import com.blackducksoftware.ohcount4j.scan.ClearSilverTemplateScanner;
import com.blackducksoftware.ohcount4j.scan.ClojureScanner;
import com.blackducksoftware.ohcount4j.scan.CobolScanner;
import com.blackducksoftware.ohcount4j.scan.CoffeeScriptScanner;
import com.blackducksoftware.ohcount4j.scan.ColdFusionScanner;
import com.blackducksoftware.ohcount4j.scan.CoqScanner;
import com.blackducksoftware.ohcount4j.scan.DScanner;
import com.blackducksoftware.ohcount4j.scan.DclScanner;
import com.blackducksoftware.ohcount4j.scan.EiffelScanner;
import com.blackducksoftware.ohcount4j.scan.ElixirScanner;
import com.blackducksoftware.ohcount4j.scan.ErlangScanner;
import com.blackducksoftware.ohcount4j.scan.FSharpScanner;
import com.blackducksoftware.ohcount4j.scan.FactorScanner;
import com.blackducksoftware.ohcount4j.scan.ForthScanner;
import com.blackducksoftware.ohcount4j.scan.FortranFixedScanner;
import com.blackducksoftware.ohcount4j.scan.FortranFreeScanner;
import com.blackducksoftware.ohcount4j.scan.GenericCodeScanner;
import com.blackducksoftware.ohcount4j.scan.HTMLScanner;
import com.blackducksoftware.ohcount4j.scan.HamlScanner;
import com.blackducksoftware.ohcount4j.scan.HaskellScanner;
import com.blackducksoftware.ohcount4j.scan.IdlPvwaveScanner;
import com.blackducksoftware.ohcount4j.scan.JspScanner;
import com.blackducksoftware.ohcount4j.scan.LispScanner;
import com.blackducksoftware.ohcount4j.scan.LogtalkScanner;
import com.blackducksoftware.ohcount4j.scan.LuaScanner;
import com.blackducksoftware.ohcount4j.scan.MakeScanner;
import com.blackducksoftware.ohcount4j.scan.MathematicaScanner;
import com.blackducksoftware.ohcount4j.scan.MatlabScanner;
import com.blackducksoftware.ohcount4j.scan.MetapostWithTexScanner;
import com.blackducksoftware.ohcount4j.scan.MetafontScanner;
import com.blackducksoftware.ohcount4j.scan.ModulaScanner;
import com.blackducksoftware.ohcount4j.scan.OCamlScanner;
import com.blackducksoftware.ohcount4j.scan.PascalScanner;
import com.blackducksoftware.ohcount4j.scan.PerlScanner;
import com.blackducksoftware.ohcount4j.scan.PhpScanner;
import com.blackducksoftware.ohcount4j.scan.PrologScanner;
import com.blackducksoftware.ohcount4j.scan.PythonScanner;
import com.blackducksoftware.ohcount4j.scan.RebolScanner;
import com.blackducksoftware.ohcount4j.scan.RexxScanner;
import com.blackducksoftware.ohcount4j.scan.RubyScanner;
import com.blackducksoftware.ohcount4j.scan.Scanner;
import com.blackducksoftware.ohcount4j.scan.SchemeScanner;
import com.blackducksoftware.ohcount4j.scan.ShellScanner;
import com.blackducksoftware.ohcount4j.scan.SmalltalkScanner;
import com.blackducksoftware.ohcount4j.scan.SqlScanner;
import com.blackducksoftware.ohcount4j.scan.TclScanner;
import com.blackducksoftware.ohcount4j.scan.TexScanner;
import com.blackducksoftware.ohcount4j.scan.VimScriptScanner;
import com.blackducksoftware.ohcount4j.scan.VisualBasicScanner;
import com.blackducksoftware.ohcount4j.scan.XmlScanner;

public enum Language implements LanguageCategory {

    /*
     * All languages must be defined here.
     * 
     * Each language must declare three mandatory properties:
     * 
     * - The language's official display name (niceName)
     * - The category of the language, one of BUILD, LOGIC, MARKUP, UNKNOWN
     * - A Scanner subclass capable of parsing this language
     */
    ACTIONSCRIPT("ActionScript", LOGIC, ActionScriptScanner.class),
    ADA("Ada", LOGIC, AdaScanner.class),
    ASPX_CSHARP("ASP.NET (C#)", LOGIC, GenericCodeScanner.class), // TODO.
    ASPX_VB("ASP.NET (VB)", LOGIC, GenericCodeScanner.class), // TODO.
    ASSEMBLY("Assembly", LOGIC, AssemblyScanner.class),
    AUGEAS("Augeas", LOGIC, AugeasScanner.class),
    AUTOCONF("Autoconf", BUILD, AutoconfScanner.class),
    AUTOMAKE("Automake", BUILD, AutomakeScanner.class),
    AWK("Awk", LOGIC, AwkScanner.class),
    BAT("Windows Batch", LOGIC, BatScanner.class),
    BFPP("Brainfuck++", LOGIC, BfkppScanner.class),
    BINARY("Binary", LOGIC, BinaryScanner.class),
    BLITZMAX("BlitzMax", LOGIC, BlitzMaxScanner.class),
    BOO("Boo", LOGIC, BooScanner.class),
    BRAINFUCK("Brainfuck", LOGIC, BfkScanner.class),
    C("C", LOGIC, CStyleScanner.class),
    CHAISCRIPT("ChaiScript", LOGIC, CStyleScanner.class),
    CLASSIC_BASIC("Classic BASIC", LOGIC, GenericCodeScanner.class), // TODO.
    CLEARSILVER("ClearSilver", LOGIC, ClearSilverTemplateScanner.class),
    CLOJURE("Clojure", LOGIC, ClojureScanner.class),
    COBOL("COBOL", LOGIC, CobolScanner.class),
    COFFEESCRIPT("CoffeeScript", LOGIC, CoffeeScriptScanner.class),
    COLDFUSION("ColdFusion", MARKUP, ColdFusionScanner.class),
    CPP("C++", LOGIC, CStyleScanner.class),
    CMake("CMake", BUILD, CMakeScanner.class),
    CSHARP("C#", LOGIC, CStyleScanner.class),
    COQ("Coq", LOGIC, CoqScanner.class),
    CSS("CSS", MARKUP, CStyleScanner.class),
    CUDA("CUDA", LOGIC, CStyleScanner.class),
    D("D", LOGIC, DScanner.class),
    DYLAN("Dylan", LOGIC, CStyleScanner.class),
    DCL("DCL", LOGIC, DclScanner.class),
    EBUILD("Ebuild", BUILD, ShellScanner.class),
    EC("eC", LOGIC, CStyleScanner.class),
    ECMASCRIPT("ECMAScript", LOGIC, CStyleScanner.class),
    EIFFEL("Eiffel", LOGIC, EiffelScanner.class),
    ELIXIR("Elixir", LOGIC, ElixirScanner.class),
    EMACSLISP("Emacs Lisp", LOGIC, LispScanner.class),
    ERLANG("Erlang", LOGIC, ErlangScanner.class),
    FACTOR("Factor", LOGIC, FactorScanner.class),
    EXHERES("Exheres", LOGIC, ShellScanner.class),
    FORTH("Forth", LOGIC, ForthScanner.class),
    FORTRANFIXED("Fortran (Fixed-Format)", LOGIC, FortranFixedScanner.class),
    FORTRANFREE("Fortran (Free-Format)", LOGIC, FortranFreeScanner.class),
    FSHARP("F#", LOGIC, FSharpScanner.class),
    GENIE("Genie", LOGIC, CStyleScanner.class),
    GLSL("OpenGL Shading Language", LOGIC, CStyleScanner.class),
    GOLANG("Go", LOGIC, CStyleScanner.class),
    GROOVY("Groovy", LOGIC, CStyleScanner.class),
    HAML("Haml", MARKUP, HamlScanner.class),
    HAXE("HaXe", LOGIC, CStyleScanner.class),
    HTML("HTML", MARKUP, HTMLScanner.class),
    HASKELL("Haskell", LOGIC, HaskellScanner.class),
    IDL_PVWAVE("IDL/PV-WAVE/GDL", LOGIC, IdlPvwaveScanner.class),
    JAM("Jam", BUILD, ShellScanner.class),
    JAVA("Java", LOGIC, CStyleScanner.class),
    JAVASCRIPT("JavaScript", LOGIC, CStyleScanner.class),
    JSP("JSP", LOGIC, JspScanner.class),
    KOTLIN("Kotlin", LOGIC, CStyleScanner.class),
    LIMBO("Limbo", LOGIC, CStyleScanner.class),
    LISP("Lisp", LOGIC, LispScanner.class),
    LOGTALK("Logtalk", LOGIC, LogtalkScanner.class),
    LUA("Lua", LOGIC, LuaScanner.class),
    MAKE("Make", BUILD, MakeScanner.class),
    MATHEMATICA("Mathematica", LOGIC, MathematicaScanner.class),
    MATLAB("Matlab", LOGIC, MatlabScanner.class),
    METAPOST("MetaPost", MARKUP, MetapostWithTexScanner.class),
    METAFONT("MetaFont", MARKUP, MetafontScanner.class),
    MODULA2("Modula 2", LOGIC, ModulaScanner.class),
    MODULA3("Modula 3", LOGIC, ModulaScanner.class),
    OBJECTIVE_C("Objective-C", LOGIC, CStyleScanner.class),
    OCAML("OCaml", LOGIC, OCamlScanner.class),
    OCTAVE("Octave", LOGIC, MatlabScanner.class), // TODO. Octave also supports # comments
    PASCAL("Pascal", LOGIC, PascalScanner.class),
    PERL("Perl", LOGIC, PerlScanner.class),
    PHP("Php", LOGIC, PhpScanner.class),
    PUPPET("Puppet", LOGIC, GenericCodeScanner.class), // TODO.
    PROLOG("Prolog", LOGIC, PrologScanner.class),
    PYTHON("Python", LOGIC, PythonScanner.class),
    R("R", LOGIC, GenericCodeScanner.class), // TODO.
    REBOL("REBOL", LOGIC, RebolScanner.class),
    REXX("Rexx", LOGIC, RexxScanner.class),
    RUBY("Ruby", LOGIC, RubyScanner.class),
    SCALA("Scala", LOGIC, CStyleScanner.class),
    SWIFT("Swift", LOGIC, CStyleScanner.class),
    SCHEME("Scheme", LOGIC, SchemeScanner.class),
    SHELL("Shell", LOGIC, ShellScanner.class),
    SMALLTALK("Smalltalk", LOGIC, SmalltalkScanner.class),
    SQL("SQL", LOGIC, SqlScanner.class),
    STRUCTURED_BASIC("Structured Basic", LOGIC, VisualBasicScanner.class),
    TCL("Tcl", LOGIC, TclScanner.class),
    TEX("TeX/LaTeX", MARKUP, TexScanner.class),
    UNKNOWN("Unknown", CATEGORY_UNKNOWN, GenericCodeScanner.class),
    VB("VisualBasic", LOGIC, VisualBasicScanner.class),
    VBSCRIPT("VBScript", LOGIC, VisualBasicScanner.class),
    VIMSCRIPT("Vimscript", LOGIC, VimScriptScanner.class),
    XML("XML", MARKUP, XmlScanner.class),
    XMLSCHEMA("XML Schema", MARKUP, XmlScanner.class),
    XSLT("XSL Transformation", MARKUP, XmlScanner.class);

    /*
     * Optional properties of languages are declared here.
     * 
     * At a minimum, a language should define one or more file
     * extensions or filenames associated with the language.
     * 
     * You may also declare additional names (beyond the uname
     * and niceName) by which the language might be known.
     * These aliases can be matched against things like Emacs
     * mode headers or shebang directives.
     */
    static {
        ACTIONSCRIPT.extension("as");
        ADA.extensions("ada", "adb");
        ASPX_CSHARP.extension("aspx");
        ASPX_VB.extension("aspx");
        ASSEMBLY.extensions("as8", "asm", "asx", "S", "z80");
        AUGEAS.extensions("aug");
        AUTOCONF.extensions("autoconf", "ac", "m4"); // m4 (unix macro processor)
        AUTOMAKE.extensions("am");
        AWK.extension("awk");
        BAT.extension("bat");
        BFPP.extensions("bfpp");
        BINARY.extensions("inc", "st");
        BLITZMAX.extension("bmx");
        BOO.extension("boo");
        BRAINFUCK.extension("bf");
        C.extensions("c", "h");
        CHAISCRIPT.extension("chai");
        CLASSIC_BASIC.extensions("b", "bas");
        CLEARSILVER.extension("cs");
        CLOJURE.extensions("clj", "cljs", "cljc");
        CMake.extensions("cmake").filename("CMakeLists.txt");
        COBOL.extension("cbl");
        COFFEESCRIPT.extension("coffee");
        COLDFUSION.extensions("cfc", "cfm");
        CPP.extensions("C", "c++", "cc", "cpp", "cxx", "H", "h", "h++", "hh", "hpp", "hxx");
        COQ.extension("v");
        CSHARP.aliases("C#", "cs").extension("cs");
        CSS.extension("css");
        CUDA.extensions("cu", "cuh");
        D.extension("d");
        DYLAN.extension("dylan");
        DCL.extension("com");
        EBUILD.extensions("ebuild", "kdebuild-1", "eclass");
        EC.extensions("ec", "eh");
        ECMASCRIPT.extension("es");
        EIFFEL.extension("e");
        ELIXIR.extensions("ex", "exs");
        EMACSLISP.extension("el");
        ERLANG.extension("erl");
        EXHERES.extensions("exheres-0", "exheres-1", "exlib");
        FACTOR.extension("factor");
        FORTH.extensions("fr", "4th");
        FORTRANFIXED.extensions("i", "f", "f03", "f08", "f77", "f90", "f95", "for", "fpp", "ftn");
        FORTRANFREE.extensions("i90", "f", "f03", "f08", "f77", "f90", "f95", "for", "fpp", "ftn");
        FSHARP.extension("fs");
        GENIE.extension("gs");
        GLSL.extensions("frag", "glsl", "vert");
        GOLANG.extensions("go");
        GROOVY.extension("groovy");
        HAML.extension("haml");
        HAXE.extension("hx");
        HTML.extensions("htm", "html");
        HASKELL.extensions("hs", "lhs");
        JAM.filenames("Jamfile", "Jamrules");
        JAVA.extension("java");
        JAVASCRIPT.alias("js").extension("js");
        JSP.extension("jsp");
        KOTLIN.extensions("kt", "kts");
        LIMBO.extensions("b", "m");
        LOGTALK.extension("lgt");
        LUA.extension("lua");
        MAKE.filename("Makefile").extensions("mk", "pro");
        MATHEMATICA.extensions("nb", "nbs");
        METAPOST.extension("mp");
        METAFONT.extensions("mf");
        MODULA2.extensions("mod", "m2");
        MODULA3.extensions("m3", "i3");
        OBJECTIVE_C.extensions("m", "h");
        OCAML.extensions("ml", "mli");
        OCTAVE.extensions("m", "octave");
        PASCAL.extensions("pas", "pp");
        PERL.extensions("pl", "pm");
        PHP.extensions("inc", "php", "phtml", "php4", "php3", "php5", "phps");
        IDL_PVWAVE.extension("pro");
        PROLOG.extension("pl");
        PUPPET.extension("pp");
        PYTHON.extension("py");
        R.extension("r");
        REBOL.extensions("r", "r3", "reb", "rebol");
        REXX.extensions("cmd", "exec", "rexx");
        RUBY.alias("jruby").extensions("rb", "ru").filenames("Rakefile", "Gemfile");
        SCALA.extensions("scala", "sc");
        SWIFT.extensions("swift");
        SCHEME.extensions("scm", "ss");
        SHELL.extensions("bash", "sh");
        SMALLTALK.extension("st");
        SQL.extension("sql");
        STRUCTURED_BASIC.extensions("b", "bas", "bi");
        TCL.extension("tcl");
        TEX.extension("tex");
        VB.extensions("bas", "frm", "frx", "vb", "vba");
        VBSCRIPT.extensions("vbs", "vbe");
        VIMSCRIPT.extension("vim").aliases("Vim Script", "VimL");
        XML.extensions("asx", "csproj", "xml", "mxml");
        XMLSCHEMA.extension("xsd");
        XSLT.extensions("xsl", "xslt");
    }

    private final String niceName;

    private final String category;

    private final Class<? extends Scanner> scannerClass;

    private final List<String> extensions;

    private final List<String> filenames;

    private final List<String> aliases;

    Language(String niceName, String category, Class<? extends Scanner> scannerClass) {
        this.niceName = niceName;
        this.category = category;
        this.scannerClass = scannerClass;
        extensions = new ArrayList<String>();
        filenames = new ArrayList<String>();
        aliases = new ArrayList<String>();
    }

    public String uname() {
        return toString().toLowerCase();
    }

    public String niceName() {
        return niceName;
    }

    public String category() {
        return category;
    }

    public Class<? extends Scanner> scannerClass() {
        return scannerClass;
    }

    public Scanner makeScanner() {
        try {
            Scanner scanner = scannerClass.newInstance();
            scanner.setDefaultLanguage(this);
            return scanner;
        } catch (InstantiationException e) {
            throw new OhcountException(e);
        } catch (IllegalAccessException e) {
            throw new OhcountException(e);
        }
    }

    private Language extension(String ext) {
        extensions.add(ext);
        return this;
    }

    private Language extensions(String... exts) {
        for (String ext : exts) {
            extension(ext);
        }
        return this;
    }

    public List<String> getExtensions() {
        return new ArrayList<String>(extensions);
    }

    private Language filename(String filename) {
        filenames.add(filename);
        return this;
    }

    private Language filenames(String... filenames) {
        for (String filename : filenames) {
            filename(filename);
        }
        return this;
    }

    public List<String> getFilenames() {
        return new ArrayList<String>(filenames);
    }

    private Language alias(String alias) {
        aliases.add(alias);
        return this;
    }

    private Language aliases(String... aliases) {
        for (String alias : aliases) {
            alias(alias);
        }
        return this;
    }

    public List<String> getAliases() {
        return new ArrayList<String>(aliases);
    }
}
