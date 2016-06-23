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

import static com.blackducksoftware.ohcount4j.Language.*;
import static com.blackducksoftware.ohcount4j.LanguageCategory.BUILD;
import static com.blackducksoftware.ohcount4j.LanguageCategory.LOGIC;
import static com.blackducksoftware.ohcount4j.LanguageCategory.MARKUP;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

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
import com.blackducksoftware.ohcount4j.scan.SchemeScanner;
import com.blackducksoftware.ohcount4j.scan.ShellScanner;
import com.blackducksoftware.ohcount4j.scan.SmalltalkScanner;
import com.blackducksoftware.ohcount4j.scan.SqlScanner;
import com.blackducksoftware.ohcount4j.scan.TclScanner;
import com.blackducksoftware.ohcount4j.scan.TexScanner;
import com.blackducksoftware.ohcount4j.scan.VimScriptScanner;
import com.blackducksoftware.ohcount4j.scan.VisualBasicScanner;
import com.blackducksoftware.ohcount4j.scan.XmlScanner;

public class LanguageTest {

    @Test(dataProvider = "testData")
    public void allTests(Language language, String niceName, String uname, String category,
            Class<?> scannerClass, List<String> extensions, List<String> filenames) {
        Assert.assertEquals(niceName, language.niceName());
        assertEquals(uname, language.uname());
        assertEquals(category, language.category());
        Assert.assertEquals(scannerClass, language.scannerClass());
        assertEquals(extensions.size(), language.getExtensions().size());
        assertTrue(language.getExtensions().containsAll(extensions));
        assertEquals(filenames.size(), language.getFilenames().size());
        assertTrue(language.getFilenames().containsAll(filenames));
    }

    @DataProvider
    public Object[][] testData() {
        return new Object[][] {
                // Language, niceName, uname, category, scannerClass, extensions, filenames
                { ACTIONSCRIPT, "ActionScript", "actionscript", LOGIC, ActionScriptScanner.class, Arrays.asList("as"), Arrays.asList() },
                { ADA, "Ada", "ada", LOGIC, AdaScanner.class,
                        Arrays.asList("ada", "adb"), Arrays.asList() },
                { ASPX_CSHARP, "ASP.NET (C#)", "aspx_csharp", LOGIC, GenericCodeScanner.class,
                        Arrays.asList("aspx"), Arrays.asList() },
                { ASPX_VB, "ASP.NET (VB)", "aspx_vb", LOGIC, GenericCodeScanner.class,
                        Arrays.asList("aspx"), Arrays.asList() },
                { ASSEMBLY, "Assembly", "assembly", LOGIC, AssemblyScanner.class,
                        Arrays.asList("as8", "asm", "asx", "S", "z80"), Arrays.asList() },
                { AUGEAS, "Augeas", "augeas", LOGIC, AugeasScanner.class,
                        Arrays.asList("aug"), Arrays.asList() },
                { AUTOCONF, "Autoconf", "autoconf", BUILD, AutoconfScanner.class,
                        Arrays.asList("autoconf", "ac", "m4"), Arrays.asList() },
                { AUTOMAKE, "Automake", "automake", BUILD, AutomakeScanner.class,
                        Arrays.asList("am"), Arrays.asList() },
                { AWK, "Awk", "awk", LOGIC, AwkScanner.class,
                        Arrays.asList("awk"), Arrays.asList() },
                { BAT, "Windows Batch", "bat", LOGIC, BatScanner.class,
                        Arrays.asList("bat"), Arrays.asList() },
                { BFPP, "Brainfuck++", "bfpp", LOGIC, BfkppScanner.class,
                        Arrays.asList("bfpp"), Arrays.asList() },
                { BINARY, "Binary", "binary", LOGIC, BinaryScanner.class,
                        Arrays.asList("inc", "st"), Arrays.asList() },
                { BLITZMAX, "BlitzMax", "blitzmax", LOGIC, BlitzMaxScanner.class,
                        Arrays.asList("bmx"), Arrays.asList() },
                { BOO, "Boo", "boo", LOGIC, BooScanner.class,
                        Arrays.asList("boo"), Arrays.asList() },
                { BRAINFUCK, "Brainfuck", "brainfuck", LOGIC, BfkScanner.class,
                        Arrays.asList("bf"), Arrays.asList() },
                { C, "C", "c", LOGIC, CStyleScanner.class,
                        Arrays.asList("c", "h"), Arrays.asList() },
                { CHAISCRIPT, "ChaiScript", "chaiscript", LOGIC, CStyleScanner.class,
                        Arrays.asList("chai"), Arrays.asList() },
                { CLASSIC_BASIC, "Classic BASIC", "classic_basic", LOGIC, GenericCodeScanner.class,
                        Arrays.asList("b", "bas"), Arrays.asList() },
                { CLEARSILVER, "ClearSilver", "clearsilver", LOGIC, ClearSilverTemplateScanner.class,
                        Arrays.asList("cs"), Arrays.asList() },
                { CLOJURE, "Clojure", "clojure", LOGIC, ClojureScanner.class,
                        Arrays.asList("clj", "cljs", "cljc"), Arrays.asList() },
                { COBOL, "COBOL", "cobol", LOGIC, CobolScanner.class,
                        Arrays.asList("cbl"), Arrays.asList() },
                { COFFEESCRIPT, "CoffeeScript", "coffeescript", LOGIC, CoffeeScriptScanner.class,
                        Arrays.asList("coffee"), Arrays.asList() },
                { COLDFUSION, "ColdFusion", "coldfusion", MARKUP, ColdFusionScanner.class,
                        Arrays.asList("cfc", "cfm"), Arrays.asList() },
                { CPP, "C++", "cpp", LOGIC, CStyleScanner.class,
                        Arrays.asList("C", "c++", "cc", "cpp", "cxx", "H", "h", "h++", "hh", "hpp", "hxx"), Arrays.asList() },
                { CMake, "CMake", "cmake", BUILD, CMakeScanner.class,
                        Arrays.asList("cmake"), Arrays.asList("CMakeLists.txt") },
                { CSHARP, "C#", "csharp", LOGIC, CStyleScanner.class,
                        Arrays.asList("cs"), Arrays.asList() },
                { COQ, "Coq", "coq", LOGIC, CoqScanner.class,
                        Arrays.asList("v"), Arrays.asList() },
                { CSS, "CSS", "css", MARKUP, CStyleScanner.class,
                        Arrays.asList("css"), Arrays.asList() },
                { CUDA, "CUDA", "cuda", LOGIC, CStyleScanner.class,
                        Arrays.asList("cu", "cuh"), Arrays.asList() },
                { D, "D", "d", LOGIC, DScanner.class,
                        Arrays.asList("d"), Arrays.asList() },
                { DYLAN, "Dylan", "dylan", LOGIC, CStyleScanner.class,
                        Arrays.asList("dylan"), Arrays.asList() },
                { DCL, "DCL", "dcl", LOGIC, DclScanner.class,
                        Arrays.asList("com"), Arrays.asList() },
                { EBUILD, "Ebuild", "ebuild", BUILD, ShellScanner.class,
                        Arrays.asList("ebuild", "eclass", "kdebuild-1"), Arrays.asList() },
                { EC, "eC", "ec", LOGIC, CStyleScanner.class,
                        Arrays.asList("ec", "eh"), Arrays.asList() },
                { ECMASCRIPT, "ECMAScript", "ecmascript", LOGIC, CStyleScanner.class,
                        Arrays.asList("es"), Arrays.asList() },
                { ELIXIR, "Elixir", "elixir", LOGIC, ElixirScanner.class,
                        Arrays.asList("ex", "exs"), Arrays.asList() },
                { EIFFEL, "Eiffel", "eiffel", LOGIC, EiffelScanner.class,
                        Arrays.asList("e"), Arrays.asList() },
                { EMACSLISP, "Emacs Lisp", "emacslisp", LOGIC, LispScanner.class,
                        Arrays.asList("el"), Arrays.asList() },
                { ERLANG, "Erlang", "erlang", LOGIC, ErlangScanner.class,
                        Arrays.asList("erl"), Arrays.asList() },
                { EXHERES, "Exheres", "exheres", LOGIC, ShellScanner.class,
                        Arrays.asList("exheres-0", "exheres-1", "exlib"), Arrays.asList() },
                { FORTH, "Forth", "forth", LOGIC, ForthScanner.class,
                        Arrays.asList("fr", "4th"), Arrays.asList() },
                { FACTOR, "Factor", "factor", LOGIC, FactorScanner.class,
                        Arrays.asList("factor"), Arrays.asList() },
                { FORTRANFIXED, "Fortran (Fixed-Format)", "fortranfixed", LOGIC, FortranFixedScanner.class,
                        Arrays.asList("i", "f", "f03", "f08", "f77", "f90", "f95", "for", "fpp", "ftn"), Arrays.asList() },
                { FORTRANFREE, "Fortran (Free-Format)", "fortranfree", LOGIC, FortranFreeScanner.class,
                        Arrays.asList("i90", "f", "f03", "f08", "f77", "f90", "f95", "for", "fpp", "ftn"), Arrays.asList() },
                { FSHARP, "F#", "fsharp", LOGIC, FSharpScanner.class,
                        Arrays.asList("fs"), Arrays.asList() },
                { GENIE, "Genie", "genie", LOGIC, CStyleScanner.class,
                        Arrays.asList("gs"), Arrays.asList() },
                { GLSL, "OpenGL Shading Language", "glsl", LOGIC, CStyleScanner.class,
                        Arrays.asList("frag", "glsl", "vert"), Arrays.asList() },
                { GOLANG, "Go", "golang", LOGIC, CStyleScanner.class,
                        Arrays.asList("go"), Arrays.asList() },
                { GROOVY, "Groovy", "groovy", LOGIC, CStyleScanner.class,
                        Arrays.asList("groovy"), Arrays.asList() },
                { HAML, "Haml", "haml", MARKUP, HamlScanner.class,
                        Arrays.asList("haml"), Arrays.asList() },
                { HAXE, "HaXe", "haxe", LOGIC, CStyleScanner.class,
                        Arrays.asList("hx"), Arrays.asList() },
                { HTML, "HTML", "html", MARKUP, HTMLScanner.class,
                        Arrays.asList("htm", "html"), Arrays.asList() },
                { HASKELL, "Haskell", "haskell", LOGIC, HaskellScanner.class,
                        Arrays.asList("hs", "lhs"), Arrays.asList() },
                { IDL_PVWAVE, "IDL/PV-WAVE/GDL", "idl_pvwave", LOGIC, IdlPvwaveScanner.class,
                            Arrays.asList("pro"), Arrays.asList() },
                { JAM, "Jam", "jam", BUILD, ShellScanner.class,
                        Arrays.asList(), Arrays.asList("Jamfile", "Jamrules") },
                { JAVA, "Java", "java", LOGIC, CStyleScanner.class,
                        Arrays.asList("java"), Arrays.asList() },
                { JAVASCRIPT, "JavaScript", "javascript", LOGIC, CStyleScanner.class,
                        Arrays.asList("js"), Arrays.asList() },
                { JSP, "JSP", "jsp", LOGIC, JspScanner.class,
                        Arrays.asList("jsp"), Arrays.asList() },
                { KOTLIN, "Kotlin", "kotlin", LOGIC, CStyleScanner.class,
                            Arrays.asList("kt", "kts"), Arrays.asList() },
                { LIMBO, "Limbo", "limbo", LOGIC, CStyleScanner.class,
                        Arrays.asList("b", "m"), Arrays.asList() },
                { LISP, "Lisp", "lisp", LOGIC, LispScanner.class,
                        Arrays.asList(), Arrays.asList() },
                { LOGTALK, "Logtalk", "logtalk", LOGIC, LogtalkScanner.class,
                        Arrays.asList("lgt"), Arrays.asList() },
                { LUA, "Lua", "lua", LOGIC, LuaScanner.class,
                        Arrays.asList("lua"), Arrays.asList() },
                { MAKE, "Make", "make", BUILD, MakeScanner.class,
                        Arrays.asList("mk", "pro"), Arrays.asList("Makefile") },
                { MATHEMATICA, "Mathematica", "mathematica", LOGIC, MathematicaScanner.class,
                        Arrays.asList("nb", "nbs"), Arrays.asList() },
                { MATLAB, "Matlab", "matlab", LOGIC, MatlabScanner.class,
                        Arrays.asList(), Arrays.asList() },
                { METAPOST, "MetaPost", "metapost", MARKUP, MetapostWithTexScanner.class,
                        Arrays.asList("mp"), Arrays.asList() },
                { METAFONT, "MetaFont", "metafont", MARKUP, MetafontScanner.class,
                            Arrays.asList("mf"), Arrays.asList() },
                { MODULA2, "Modula 2", "modula2", LOGIC, ModulaScanner.class,
                        Arrays.asList("mod", "m2"), Arrays.asList() },
                { MODULA3, "Modula 3", "modula3", LOGIC, ModulaScanner.class,
                        Arrays.asList("m3", "i3"), Arrays.asList() },
                { OBJECTIVE_C, "Objective-C", "objective_c", LOGIC, CStyleScanner.class,
                        Arrays.asList("m", "h"), Arrays.asList() },
                { OCAML, "OCaml", "ocaml", LOGIC, OCamlScanner.class,
                        Arrays.asList("ml", "mli"), Arrays.asList() },
                { OCTAVE, "Octave", "octave", LOGIC, MatlabScanner.class,
                        Arrays.asList("m", "octave"), Arrays.asList() },
                { PASCAL, "Pascal", "pascal", LOGIC, PascalScanner.class,
                        Arrays.asList("pas", "pp"), Arrays.asList() },
                { PERL, "Perl", "perl", LOGIC, PerlScanner.class,
                        Arrays.asList("pl", "pm"), Arrays.asList() },
                { PHP, "Php", "php", LOGIC, PhpScanner.class,
                        Arrays.asList("inc", "php", "phtml", "php4", "php3", "php5", "phps"), Arrays.asList() },
                { PUPPET, "Puppet", "puppet", LOGIC, GenericCodeScanner.class,
                        Arrays.asList("pp"), Arrays.asList() },
                { PROLOG, "Prolog", "prolog", LOGIC, PrologScanner.class,
                        Arrays.asList("pl"), Arrays.asList() },
                { PYTHON, "Python", "python", LOGIC, PythonScanner.class,
                        Arrays.asList("py"), Arrays.asList() },
                { R, "R", "r", LOGIC, GenericCodeScanner.class,
                        Arrays.asList("r"), Arrays.asList() },
                { REBOL, "REBOL", "rebol", LOGIC, RebolScanner.class,
                        Arrays.asList("r", "r3", "reb", "rebol"), Arrays.asList() },
                { REXX, "Rexx", "rexx", LOGIC, RexxScanner.class,
                        Arrays.asList("cmd", "exec", "rexx"), Arrays.asList() },
                { RUBY, "Ruby", "ruby", LOGIC, RubyScanner.class,
                        Arrays.asList("rb", "ru"), Arrays.asList("Rakefile", "Gemfile") },
                { SCALA, "Scala", "scala", LOGIC, CStyleScanner.class,
                        Arrays.asList("scala", "sc"), Arrays.asList() },
                { SWIFT, "Swift", "swift", LOGIC, CStyleScanner.class,
                        Arrays.asList("swift"), Arrays.asList() },
                { SCHEME, "Scheme", "scheme", LOGIC, SchemeScanner.class,
                        Arrays.asList("scm", "ss"), Arrays.asList() },
                { SHELL, "Shell", "shell", LOGIC, ShellScanner.class,
                        Arrays.asList("bash", "sh"), Arrays.asList() },
                { SMALLTALK, "Smalltalk", "smalltalk", LOGIC, SmalltalkScanner.class,
                        Arrays.asList("st"), Arrays.asList() },
                { SQL, "SQL", "sql", LOGIC, SqlScanner.class,
                        Arrays.asList("sql"), Arrays.asList() },
                { STRUCTURED_BASIC, "Structured Basic", "structured_basic", LOGIC, VisualBasicScanner.class,
                        Arrays.asList("b", "bas", "bi"), Arrays.asList() },
                { TCL, "Tcl", "tcl", LOGIC, TclScanner.class,
                        Arrays.asList("tcl"), Arrays.asList() },
                { TEX, "TeX/LaTeX", "tex", MARKUP, TexScanner.class,
                        Arrays.asList("tex"), Arrays.asList() },
                { VB, "VisualBasic", "vb", LOGIC, VisualBasicScanner.class,
                        Arrays.asList("bas", "frm", "frx", "vb", "vba"), Arrays.asList() },
                { VBSCRIPT, "VBScript", "vbscript", LOGIC, VisualBasicScanner.class,
                        Arrays.asList("vbs", "vbe"), Arrays.asList() },
                { VIMSCRIPT, "Vimscript", "vimscript", LOGIC, VimScriptScanner.class,
                        Arrays.asList("vim"), Arrays.asList() },
                { XML, "XML", "xml", MARKUP, XmlScanner.class,
                        Arrays.asList("asx", "csproj", "xml", "mxml"), Arrays.asList() },
                { XMLSCHEMA, "XML Schema", "xmlschema", MARKUP, XmlScanner.class,
                        Arrays.asList("xsd"), Arrays.asList() },
                { XSLT, "XSL Transformation", "xslt", MARKUP, XmlScanner.class,
                        Arrays.asList("xsl", "xslt"), Arrays.asList() }
        };
    }

    @Test
    public void testNoLanguageMissed() {
        Object[][] testDatas = testData();
        Set<Language> notFoundLang = new LinkedHashSet<Language>();
        Set<Language> excludeLang = new HashSet<Language>();
        excludeLang.add(UNKNOWN);
        for (Language l : Language.values()) {
            boolean found = false;
            for (Object[] data : testDatas) {
                if (data[0] == l) {
                    found = true;
                    break;
                }
            }
            if (!found && !excludeLang.contains(l)) {
                notFoundLang.add(l);
            }
        }
        Assert.assertEquals(notFoundLang.size(), 0, "testData missing for Languages : " + notFoundLang);
    }

}
