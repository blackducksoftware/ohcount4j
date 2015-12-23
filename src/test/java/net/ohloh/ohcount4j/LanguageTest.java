package net.ohloh.ohcount4j;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import net.ohloh.ohcount4j.scan.AugeasScanner;
import net.ohloh.ohcount4j.scan.AutoconfScanner;
import net.ohloh.ohcount4j.scan.AutomakeScanner;
import net.ohloh.ohcount4j.scan.AwkScanner;
import net.ohloh.ohcount4j.scan.BatScanner;
import net.ohloh.ohcount4j.scan.BfkScanner;
import net.ohloh.ohcount4j.scan.BfkppScanner;
import net.ohloh.ohcount4j.scan.CStyleScanner;
import net.ohloh.ohcount4j.scan.ClojureScanner;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

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
                { Language.GOLANG, "golang" }, // GoLang
                { Language.RUBY, "ruby" },
                { Language.AWK, "awk" },
                { Language.SCALA, "scala" },
                { Language.SWIFT, "swift" },
                { Language.BAT, "bat" }
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
                { Language.CLOJURE, Arrays.asList("clj", "cljs", "cljc", "edn") },
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
                { Language.MAKE, Arrays.asList("Makefile") }
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
                { Language.CLOJURE, LanguageCategory.LOGIC },
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
                { Language.CLOJURE, ClojureScanner.class },
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
                { Language.BRAINFUCK, "Brainfuck" },
                { Language.CLOJURE, "Clojure" },
                { Language.GOLANG, "Go" },
                { Language.SCALA, "Scala" },
                { Language.SWIFT, "Swift" }
        };
    }

}
