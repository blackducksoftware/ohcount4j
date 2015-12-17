package net.ohloh.ohcount4j;

import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.scan.ActionScriptScanner;
import net.ohloh.ohcount4j.scan.AdaScanner;
import net.ohloh.ohcount4j.scan.AssemblyScanner;
import net.ohloh.ohcount4j.scan.AugeasScanner;
import net.ohloh.ohcount4j.scan.AutoconfScanner;
import net.ohloh.ohcount4j.scan.BinaryScanner;
import net.ohloh.ohcount4j.scan.BooScanner;
import net.ohloh.ohcount4j.scan.CStyleScanner;
import net.ohloh.ohcount4j.scan.CobolScanner;
import net.ohloh.ohcount4j.scan.ColdFusionScanner;
import net.ohloh.ohcount4j.scan.DScanner;
import net.ohloh.ohcount4j.scan.EiffelScanner;
import net.ohloh.ohcount4j.scan.ErlangScanner;
import net.ohloh.ohcount4j.scan.FSharpScanner;
import net.ohloh.ohcount4j.scan.FortranFixedScanner;
import net.ohloh.ohcount4j.scan.FortranFreeScanner;
import net.ohloh.ohcount4j.scan.GenericCodeScanner;
import net.ohloh.ohcount4j.scan.HTMLScanner;
import net.ohloh.ohcount4j.scan.HaskellScanner;
import net.ohloh.ohcount4j.scan.JspScanner;
import net.ohloh.ohcount4j.scan.LispScanner;
import net.ohloh.ohcount4j.scan.LuaScanner;
import net.ohloh.ohcount4j.scan.MakeScanner;
import net.ohloh.ohcount4j.scan.MathematicaScanner;
import net.ohloh.ohcount4j.scan.MatlabScanner;
import net.ohloh.ohcount4j.scan.ModulaScanner;
import net.ohloh.ohcount4j.scan.OCamlScanner;
import net.ohloh.ohcount4j.scan.PascalScanner;
import net.ohloh.ohcount4j.scan.PerlScanner;
import net.ohloh.ohcount4j.scan.PhpScanner;
import net.ohloh.ohcount4j.scan.PrologScanner;
import net.ohloh.ohcount4j.scan.PythonScanner;
import net.ohloh.ohcount4j.scan.RebolScanner;
import net.ohloh.ohcount4j.scan.RexxScanner;
import net.ohloh.ohcount4j.scan.RubyScanner;
import net.ohloh.ohcount4j.scan.Scanner;
import net.ohloh.ohcount4j.scan.SchemeScanner;
import net.ohloh.ohcount4j.scan.ShellScanner;
import net.ohloh.ohcount4j.scan.SmalltalkScanner;
import net.ohloh.ohcount4j.scan.SqlScanner;
import net.ohloh.ohcount4j.scan.TclScanner;
import net.ohloh.ohcount4j.scan.TexScanner;
import net.ohloh.ohcount4j.scan.VimScriptScanner;
import net.ohloh.ohcount4j.scan.VisualBasicScanner;
import net.ohloh.ohcount4j.scan.XmlScanner;

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
    BINARY("Binary", LOGIC, BinaryScanner.class),
    BOO("Boo", LOGIC, BooScanner.class),
    C("C", LOGIC, CStyleScanner.class),
    CLASSIC_BASIC("Classic BASIC", LOGIC, GenericCodeScanner.class), // TODO.
    COBOL("COBOL", LOGIC, CobolScanner.class),
    COLDFUSION("ColdFusion", MARKUP, ColdFusionScanner.class),
    CPP("C++", LOGIC, CStyleScanner.class),
    CSHARP("C#", LOGIC, CStyleScanner.class),
    CSS("CSS", MARKUP, CStyleScanner.class),
    D("D", LOGIC, DScanner.class),
    ECMASCRIPT("ECMAScript", LOGIC, CStyleScanner.class),
    EIFFEL("Eiffel", LOGIC, EiffelScanner.class),
    ERLANG("Erlang", LOGIC, ErlangScanner.class),
    FORTRANFIXED("Fortran (Fixed-Format)", LOGIC, FortranFixedScanner.class),
    FORTRANFREE("Fortran (Free-Format)", LOGIC, FortranFreeScanner.class),
    FSHARP("F#", LOGIC, FSharpScanner.class),
    GOLANG("Go", LOGIC, CStyleScanner.class),
    GROOVY("Groovy", LOGIC, CStyleScanner.class),
    HTML("HTML", MARKUP, HTMLScanner.class),
    HASKELL("Haskell", LOGIC, HaskellScanner.class),
    JAVA("Java", LOGIC, CStyleScanner.class),
    JAVASCRIPT("JavaScript", LOGIC, CStyleScanner.class),
    LIMBO("Limbo", LOGIC, CStyleScanner.class),
    JSP("JSP", LOGIC, JspScanner.class),
    LISP("Lisp", LOGIC, LispScanner.class),
    LUA("Lua", LOGIC, LuaScanner.class),
    MAKE("Make", BUILD, MakeScanner.class),
    MATHEMATICA("Mathematica", LOGIC, MathematicaScanner.class),
    MATLAB("Matlab", LOGIC, MatlabScanner.class),
    MODULA2("Modula 2", LOGIC, ModulaScanner.class),
    MODULA3("Modula 3", LOGIC, ModulaScanner.class),
    OBJECTIVE_C("Objective-C", LOGIC, CStyleScanner.class),
    OCAML("OCaml", LOGIC, OCamlScanner.class),
    OCTAVE("Octave", LOGIC, MatlabScanner.class), // TODO. Octave also supports # comments
    PASCAL("Pascal", LOGIC, PascalScanner.class),
    PERL("Perl", LOGIC, PerlScanner.class),
    PHP("Php", LOGIC, PhpScanner.class),
    PUPPET("Puppet", LOGIC, GenericCodeScanner.class), // TODO.
    PVWAVE("IDL/PV-WAVE/GDL", LOGIC, GenericCodeScanner.class), // TODO.
    PROLOG("Prolog", LOGIC, PrologScanner.class),
    PYTHON("Python", LOGIC, PythonScanner.class),
    R("R", LOGIC, GenericCodeScanner.class), // TODO.
    REBOL("REBOL", LOGIC, RebolScanner.class),
    REXX("Rexx", LOGIC, RexxScanner.class),
    RUBY("Ruby", LOGIC, RubyScanner.class),
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
        BINARY.extensions("inc", "st");
        BOO.extension("boo");
        C.extensions("c", "h");
        CLASSIC_BASIC.extensions("b", "bas");
        COBOL.extension("cbl");
        COLDFUSION.extensions("cfc", "cfm");
        CPP.extensions("C", "c++", "cc", "cpp", "cxx", "H", "h", "h++", "hh", "hpp", "hxx");
        CSHARP.aliases("C#", "cs").extension("cs");
        CSS.extension("css");
        D.extension("d");
        ECMASCRIPT.extension("es");
        EIFFEL.extension("e");
        ERLANG.extension("erl");
        FORTRANFIXED.extensions("i", "f", "f03", "f08", "f77", "f90", "f95", "for", "fpp", "ftn");
        FORTRANFREE.extensions("i90", "f", "f03", "f08", "f77", "f90", "f95", "for", "fpp", "ftn");
        FSHARP.extension("fs");
        GOLANG.extensions("go");
        GROOVY.extension("groovy");
        HTML.extensions("htm", "html");
        HASKELL.extensions("hs", "lhs");
        JAVA.extension("java");
        JAVASCRIPT.alias("js").extension("js");
        JSP.extension("jsp");
        LIMBO.extensions("b", "m");
        LUA.extension("lua");
        MAKE.filename("Makefile").extensions("mk", "pro");
        MATHEMATICA.extensions("nb", "nbs");
        MODULA2.extensions("mod", "m2");
        MODULA3.extensions("m3", "i3");
        OBJECTIVE_C.extensions("m", "h");
        OCAML.extensions("ml", "mli");
        OCTAVE.extensions("m", "octave");
        PASCAL.extensions("pas", "pp");
        PERL.extensions("pl", "pm");
        PHP.extensions("inc", "php", "phtml", "php4", "php3", "php5", "phps");
        PVWAVE.extension("pro");
        PROLOG.extension("pl");
        PUPPET.extension("pp");
        PYTHON.extension("py");
        R.extension("r");
        REBOL.extensions("r", "r3", "reb", "rebol");
        REXX.extensions("cmd", "exec", "rexx");
        RUBY.alias("jruby").extensions("rb", "ru").filenames("Rakefile", "Gemfile");
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

    private List<String> extensions;

    private List<String> filenames;

    private List<String> aliases;

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

    public Language extension(String ext) {
        extensions.add(ext);
        return this;
    }

    public Language extensions(String... exts) {
        for (String ext : exts) {
            extension(ext);
        }
        return this;
    }

    public List<String> getExtensions() {
        return extensions;
    }

    public Language filename(String filename) {
        filenames.add(filename);
        return this;
    }

    public Language filenames(String... filenames) {
        for (String filename : filenames) {
            filename(filename);
        }
        return this;
    }

    public List<String> getFilenames() {
        return filenames;
    }

    public Language alias(String alias) {
        aliases.add(alias);
        return this;
    }

    public Language aliases(String... aliases) {
        for (String alias : aliases) {
            alias(alias);
        }
        return this;
    }

    public List<String> getAliases() {
        return aliases;
    }
}
