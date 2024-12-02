{ inputPath, pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib;
  inherit (lib) trivial;
  inherit (lib) strings;
  inherit (lib) lists;
in
with builtins;

let
  sortedLists = trivial.pipe inputPath [
    readFile
    (split "([^\n]+)") # splitting this way, gives us the lines as matches in singleton lists, interleaved with non-matching strings
    (filter isList) # discard the non-matching strings
    (map head) # unwrap the singleton lists
    (map (line:
      trivial.pipe line [
        (split " +")
        (filter isString)
        (map strings.toInt)
      ]))
    (foldl' (acc: e: {
      a = acc.a ++ [(elemAt e 0)];
      b = acc.b ++ [(elemAt e 1)];
    }) { a=[]; b=[]; })
    ({ a, b }: {
      a = sort lessThan a;
      b = sort lessThan b;
    })
  ];

  distanceBetweenLists = (a: b:
    foldl' add 0
    (lists.zipListsWith (a: b: ((trivial.compare b a) * (b - a))) a b) # `compare` returns -1, 1, or 0
  );

  similarityScore = (a: b:
    let count = foldl' (acc: e: acc // { ${toString e} = (acc.${toString e} or 0) + 1; } ) {} b; in
    foldl' (acc: e: acc + e * count.${toString e} or 0) 0 a
  );
in
{
  part1 = with sortedLists; distanceBetweenLists a b;
  part2 = with sortedLists; similarityScore a b;
}
