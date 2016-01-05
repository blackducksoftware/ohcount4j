package com.blackducksoftware.ohcount4j;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.scan.AugeasScanner;
import com.blackducksoftware.ohcount4j.scan.AutoconfScanner;
import com.blackducksoftware.ohcount4j.scan.AutomakeScanner;
import com.blackducksoftware.ohcount4j.scan.AwkScanner;
import com.blackducksoftware.ohcount4j.scan.BatScanner;
import com.blackducksoftware.ohcount4j.scan.BfkScanner;
import com.blackducksoftware.ohcount4j.scan.BfkppScanner;
import com.blackducksoftware.ohcount4j.scan.BlitzMaxScanner;
import com.blackducksoftware.ohcount4j.scan.CStyleScanner;
import com.blackducksoftware.ohcount4j.scan.ClojureScanner;
import com.blackducksoftware.ohcount4j.scan.CoqScanner;

public class LanguageTest {

    @Test(dataProvider = "uname")
    public void unameTest(Language language, String uname) {
        assertEquals(uname, language.uname());
    }

    @Test(dataProvider = "extensions")
    public void extensionsTest(Language language, List<String> extensions) {
        assertEquals(extensions.size(), language.getExtensions().size());
        assertTrue(language.getExtensions().containsAll(extensions));
    }

    @Test(dataProvider = "filenames")
    public void filenamesTest(Language language, List<String> filenames) {
        assertEquals(filenames.size(), language.getFilenames().size());
        assertTrue(language.getFilenames().containsAll(filenames));
    }

    @Test(dataProvider = "category")
    public void categoryTest(Language language, String category) {
        assertEquals(category, language.category());
    }

    @Test(dataProvider = "scannerClass")
    public void scannerClassTest(Language language, Class<?> scannerClass) {
        Assert.assertEquals(scannerClass, language.scannerClass());
    }

    @Test(dataProvider = "niceName")
    public void niceNameTest(Language language, String niceName) {
        Assert.assertEquals(niceName, language.niceName());
    }

    @DataProvider
    public Object[][] uname() {
        return new Object[][] {
                { Language.AUGEAS, "augeas" }, // Augeas
                { Language.AUTOCONF, "autoconf", },
                { Language.AWK, "awk", },
                { Language.BRAINFUCK, "brainfuck" },
                { Language.BFPP, "bfpp" },
                { Language.C, "c" },
                { Language.CLOJURE, "clojure" },
                { Language.COQ, "coq" },
                { Language.GOLANG, "golang" }, // GoLang
                { Language.RUBY, "ruby" },
                { Language.AWK, "awk" },
                { Language.SCALA, "scala" },
                { Language.SWIFT, "swift" },
                { Language.BAT, "bat" },
                { Language.CHAISCRIPT, "chaiscript" },
                { Language.BLITZMAX, "blitzmax" },
        };
    }

    @DataProvider
    public Object[][] extensions() {
        return new Object[][] {
                { Language.AUGEAS, Arrays.asList("aug") },
                { Language.AUTOCONF, Arrays.asList("ac", "autoconf", "m4") },
                { Language.AUTOMAKE, Arrays.asList("am") },
                { Language.AWK, Arrays.asList("awk") },
                { Language.BFPP, Arrays.asList("bfpp") },
                { Language.BRAINFUCK, Arrays.asList("bf") },
                { Language.BAT, Arrays.asList("bat") },
                { Language.BLITZMAX, Arrays.asList("bmx") },
                { Language.CHAISCRIPT, Arrays.asList("chai") },
                { Language.CLOJURE, Arrays.asList("clj", "cljs", "cljc") },
                { Language.COQ, Arrays.asList("v") },
                { Language.GOLANG, Arrays.asList("go") },
                { Language.RUBY, Arrays.asList("rb", "ru") },
                { Language.SCALA, Arrays.asList("scala", "sc") },
                { Language.SWIFT, Arrays.asList("swift") }

        };
    }

    @DataProvider
    public Object[][] filenames() {
        return new Object[][] {
                { Language.RUBY, Arrays.asList("Rakefile", "Gemfile") },
                { Language.MAKE, Arrays.asList("Makefile") },
                { Language.CMake, Arrays.asList("CMakeLists.txt") }
        };
    }

    @DataProvider
    public Object[][] category() {
        return new Object[][] {
                { Language.AUGEAS, LanguageCategory.LOGIC },
                { Language.AUTOCONF, LanguageCategory.BUILD },
                { Language.AUTOMAKE, LanguageCategory.BUILD },
                { Language.AWK, LanguageCategory.LOGIC },
                { Language.BAT, LanguageCategory.LOGIC },
                { Language.BFPP, LanguageCategory.LOGIC },
                { Language.BRAINFUCK, LanguageCategory.LOGIC },
                { Language.BLITZMAX, LanguageCategory.LOGIC },
                { Language.CHAISCRIPT, LanguageCategory.LOGIC },
                { Language.CLOJURE, LanguageCategory.LOGIC },
                { Language.COQ, LanguageCategory.LOGIC },
                { Language.GOLANG, LanguageCategory.LOGIC },
                { Language.SCALA, LanguageCategory.LOGIC },
                { Language.SWIFT, LanguageCategory.LOGIC }
        };
    }

    @DataProvider
    public Object[][] scannerClass() {
        return new Object[][] {
                { Language.AUGEAS, AugeasScanner.class },
                { Language.AUTOCONF, AutoconfScanner.class },
                { Language.AUTOMAKE, AutomakeScanner.class },
                { Language.AWK, AwkScanner.class },
                { Language.BAT, BatScanner.class },
                { Language.BFPP, BfkppScanner.class },
                { Language.BRAINFUCK, BfkScanner.class },
                { Language.BLITZMAX, BlitzMaxScanner.class },
                { Language.CHAISCRIPT, CStyleScanner.class },
                { Language.CLOJURE, ClojureScanner.class },
                { Language.COQ, CoqScanner.class },
                { Language.GOLANG, CStyleScanner.class },
                { Language.SCALA, CStyleScanner.class },
                { Language.SWIFT, CStyleScanner.class }
        };
    }

    @DataProvider
    public Object[][] niceName() {
        return new Object[][] {
                { Language.AUGEAS, "Augeas" },
                { Language.AUTOCONF, "Autoconf" },
                { Language.AUTOMAKE, "Automake" },
                { Language.AWK, "Awk" },
                { Language.BFPP, "Brainfuck++" },
                { Language.BLITZMAX, "BlitzMax" },
                { Language.BRAINFUCK, "Brainfuck" },
                { Language.CHAISCRIPT, "ChaiScript" },
                { Language.CLOJURE, "Clojure" },
                { Language.COQ, "Coq" },
                { Language.GOLANG, "Go" },
                { Language.SCALA, "Scala" },
                { Language.SWIFT, "Swift" }
        };
    }

    @Test
    public void testCategory() {
        Assert.assertEquals(Language.AUGEAS.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.AUTOCONF.category(), LanguageCategory.BUILD);
        Assert.assertEquals(Language.AUTOMAKE.category(), LanguageCategory.BUILD);
        Assert.assertEquals(Language.AWK.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.BAT.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.BFPP.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.BRAINFUCK.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.CMake.category(), LanguageCategory.BUILD);
        Assert.assertEquals(Language.CHAISCRIPT.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.CLOJURE.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.COQ.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.GOLANG.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.SCALA.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.SWIFT.category(), LanguageCategory.LOGIC);
    }

}
