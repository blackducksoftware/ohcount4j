package net.ohloh.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnPLResolverTest {

    private ExtnPLResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnPLResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.PERL));
        assertTrue(r.canResolve(Language.PROLOG));
    }

    @Test
    public void perlByDefaultTest() throws IOException {
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "")));
    }

    @Test
    public void perlShebangTest() throws IOException {
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "#!/usr/bin/env perl")));
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "#!/usr/bin/perl")));
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "#!%PERL%")));
    }

    @Test
    public void prologRuleTest() throws IOException {
        assertEquals(Language.PROLOG, r.resolve(new SourceFile("foo.pl", "Foo :- Bar")));
        assertEquals(Language.PROLOG, r.resolve(new SourceFile("foo.pl", "Foo :-\n")));
    }

    @Test
    public void smileysAreNotPrologTest() throws IOException {
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl",
                "# This is a Perl file, even though a smiley :-) looks like Prolog\n")));
    }

    @Test
    public void perlShebangWithPrologBodyTest() throws IOException {
        // The body appears to contain Prolog, but the shebang specifies Perl,
        // so we respect the shebang and choose Perl.
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl",
                "#!%PERL%\n" +
                        "Foo :- Bar.\n")));
    }
}
