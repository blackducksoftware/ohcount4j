/*
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 * 
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package com.blackducksoftware.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.nio.CharBuffer;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.google.common.base.CharMatcher;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

/**
 * @author mpujari
 *
 */
public class DiffSourceFile {

    private static final int HALFLONG = 16;

    private final SourceFile from;

    private final SourceFile to;

    private Map<Integer, List<LineHash>> file = Maps.newHashMap();

    private int sfile[] = new int[2];

    private int slen[] = new int[2];

    public DiffSourceFile(SourceFile from, SourceFile to) {
        this.from = from;
        this.to = to;
    }

    /*
     * This implementation is taken from ohcount
     * 
     * Can be found at ohcount/src/diff.c
     */
    public Diff diff() throws IOException {
        prepare(0, from.getContents());
        prepare(1, to.getContents());
        prune();

        sort(0);
        sort(1);

        equiv();

        unsort();

        printValues(file.get(0), 3);
        printValues(file.get(1), 3);

        return null;
    }

    private void unsort() {
        List<LineHash> b = file.get(0);
        int sfileIndex0 = sfile[0];
        int l = slen[0];
        int[] a = new int[l + 1 * 2];
        int i;

        for (i = 1; i <= l; i++) {
            a[b.get(i + sfileIndex0).serial] = b.get(i + sfileIndex0).value;
        }

        int counter = 1;
        for (i = 1; i <= l; i++) {
            if (counter++ % 2 == 0) {
                b.get(i / 2).serial = a[i];
            } else {
                b.get(i / 2).value = a[i];
            }
        }
    }

    private void equiv() {
        int sfileIndex0 = sfile[0];
        int sfileIndex1 = sfile[1];
        int n = slen[0];
        int m = slen[1];
        int i, j;

        List<LineHash> a = file.get(0);
        List<LineHash> b = file.get(1);
        i = j = 1;
        while (i <= n && j <= m) {
            if (a.get(i + sfileIndex0).value < b.get(j + sfileIndex1).value) {
                a.get(i++ + sfileIndex0).value = 0;
            } else if (a.get(i + sfileIndex0).value == b.get(j + sfileIndex1).value) {
                a.get(i++ + sfileIndex0).value = j;
            } else {
                j++;
            }
        }
        while (i <= n) {
            a.get(i++ + sfileIndex0).value = 0;
        }
        b.get(m + 1 + sfileIndex1).value = 0;
        j = 0;
        int counter = 1;
        while (++j <= m) {
            if (counter++ % 2 == 0) {
                b.get(j / 2).serial = -b.get(j + sfileIndex1).serial;
            } else {
                b.get(j / 2).value = -b.get(j + sfileIndex1).serial;
            }
            while (b.get(j + 1 + sfileIndex1).value == b.get(j + sfileIndex1).value) {
                j++;
                b.get(j).serial = b.get(j + sfileIndex1).serial;
            }
        }
        if (counter++ % 2 == 0) {
            b.get(j / 2).serial = -1;
        } else {
            b.get(j / 2).value = -1;
        }
    }

    private void printValues(List<LineHash> lines, int onlyValue) {
        for (LineHash l : lines) {
            switch (onlyValue) {
            case 1:
                System.out.print(l.value + ", ");
                break;
            case 2:
                System.out.print(l.serial + ", ");
                break;
            default:
                System.out.print("[" + l.serial + "|" + l.value + "] ");
            }
        }
        System.out.println();
    }

    private void sort(int index) {
        int sfileIndex = sfile[index];
        int n = slen[index];
        int m = 0;

        for (int i = 1; i <= n; i *= 2) {
            m = 2 * i - 1;
        }

        List<LineHash> lineHashList = file.get(index);
        for (m /= 2; m != 0; m /= 2) {
            int k = sfileIndex + (n - m);
            for (int j = sfileIndex + 1; j <= k; j++) {
                int ai = j;
                int aim = ai + m;
                // LineHash aiL = ;
                // LineHash aimL = ;
                do {
                    if (lineHashList.get(aim).value > lineHashList.get(ai).value || lineHashList.get(aim).value == lineHashList.get(ai).value
                            && lineHashList.get(aim).serial > lineHashList.get(ai).serial) {
                        break;
                    }
                    swapFiles(index, ai, aim);
                    aim = ai;
                    ai -= m;
                } while (ai > sfileIndex && aim >= ai);
            }
        }

    }

    private void swapFiles(int index, int ai, int aim) {
        List<LineHash> list = file.get(index);
        LineHash ail = list.get(ai);
        list.set(ai, list.get(aim));
        list.set(aim, ail);
    }

    private void prepare(int i, char[] bufChars) {
        char[] bufcpyChars = new char[bufChars.length];
        System.arraycopy(bufChars, 0, bufcpyChars, 0, bufChars.length);
        CharBuffer bufcpy = CharBuffer.wrap(bufcpyChars);
        List<LineHash> p = Lists.newArrayList();
        Splitter splitter = Splitter.on(CharMatcher.is('\n')).omitEmptyStrings();
        Iterable<String> iterable = splitter.split(bufcpy);
        int xv = -20;
        LineHash lh = new LineHash();
        p.add(lh);
        lh.serial = --xv;
        lh.value = xv;
        for (String str : iterable) {
            LineHash lineHash = new LineHash();
            lineHash.serial = --xv;
            lineHash.value = hash(str);
            p.add(lineHash);
        }
        file.put(i, p);
    }

    private void prune() {
        int i, j, pref = 0, suff = 0;
        int size0 = file.get(0).size() - 1;
        int size1 = file.get(1).size() - 1;
        while (pref < size0
                && pref < size1
                && file.get(0).get(pref + 1).value == file.get(1).get(pref + 1).value) {
            pref++;
        }
        while (suff < size0 - pref && suff < size1 - pref
                && file.get(0).get(size0 - suff).value == file.get(1).get(size1 - suff).value) {
            suff++;
        }
        for (j = 0; j < 2; j++) {
            sfile[j] = pref;
            slen[j] = file.get(j).size() - pref - suff - 1;
            for (i = 0; i < slen[j] + 1; i++) {
                file.get(j).get(i + pref).serial = i;
            }
        }
    }

    /**
     * Returns a computed hash for a given string.
     * Hashing has the effect of arranging line in 7-bit bytes and then summing 1-s
     * complement in 16-bit hunks.
     *
     * @param line
     *            The line of a buffer to hash.
     */
    private int hash(CharSequence line) {
        long sum = 1;
        int shift = 0;
        int strLen = line.length();
        for (int i = 0; i < strLen; i++) {
            char c = line.charAt(i);
            sum += (long) c << (shift &= (HALFLONG - 1));
            shift += 7;
        }
        sum = (sum & ((1L << HALFLONG) - 1)) + (sum >> HALFLONG);
        return ((short) (sum & ((1L << HALFLONG) - 1)) + (short) (sum >> HALFLONG));
    }

    private static class LineHash {

        private static final Random RANDOM = new Random(System.nanoTime());

        int serial = -1 * RANDOM.nextInt(Integer.MAX_VALUE);

        int value;

        @Override
        public String toString() {
            return String.format("serial:%1$d\tvalue:%2$d", serial, value);
        }

    }

    public static void main(String[] args) throws Exception {
        SourceFile from = new SourceFile(new File(args[0]));
        SourceFile to = new SourceFile(new File(args[1]));
        new DiffSourceFile(from, to).diff();
    }

}
