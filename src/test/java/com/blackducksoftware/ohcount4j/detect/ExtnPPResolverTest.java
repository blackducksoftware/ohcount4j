package com.blackducksoftware.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPPResolverTest {

    private ExtnPPResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnPPResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.PASCAL));
        assertTrue(r.canResolve(Language.PUPPET));
    }

    @Test
    public void puppetExamples() throws IOException {
        assertEquals(Language.PUPPET, r.resolve(new SourceFile("foo.pp",
                "package { \"foo\":\n" +
                        "    ensure => installed\n" +
                        "}\n")));

        assertEquals(Language.PUPPET, r.resolve(new SourceFile("foo.pp",
                "node foo {\n" +
                        "    include bar\n" +
                        "}\n")));

        assertEquals(Language.PUPPET, r.resolve(new SourceFile("foo.pp",
                "class foo {\n" +
                        "}\n")));

        assertEquals(Language.PUPPET, r.resolve(new SourceFile("foo.pp",
                "define foo (\n" +
                        ")\n")));
    }

    @Test
    public void pascalExamples() throws IOException {
        assertEquals(Language.PASCAL, r.resolve(new SourceFile("foo.pp",
                "Program FooDemo;\n" +
                        "Const Foo = {$INCLUDE %FOO%};\n")));

        assertEquals(Language.PASCAL, r.resolve(new SourceFile("foo.pp",
                "Program FooDemo;\n" +
                        "Const Foo = {$include %FOO%};\n")));

        assertEquals(Language.PASCAL, r.resolve(new SourceFile("foo.pp",
                "foo begin\n" +
                        "end.\n")));
    }

    @Test
    public void pascalByDefault() throws IOException {
        assertEquals(Language.PASCAL, r.resolve(new SourceFile("foo.pp", "")));
    }
}
