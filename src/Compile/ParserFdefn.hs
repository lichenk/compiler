{-# OPTIONS_GHC -w #-}
module Compile.ParserFdefn where

import Compile.Lexer
import Compile.Types.Ops
import Compile.Types.FAST
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27

action_0 (42) = happyShift action_15
action_0 (47) = happyShift action_5
action_0 (55) = happyShift action_6
action_0 (56) = happyShift action_7
action_0 (57) = happyShift action_8
action_0 (58) = happyShift action_16
action_0 (4) = happyGoto action_9
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_10
action_0 (10) = happyGoto action_11
action_0 (11) = happyGoto action_12
action_0 (12) = happyGoto action_13
action_0 (16) = happyGoto action_14
action_0 _ = happyFail

action_1 (42) = happyShift action_4
action_1 (47) = happyShift action_5
action_1 (55) = happyShift action_6
action_1 (56) = happyShift action_7
action_1 (57) = happyShift action_8
action_1 (5) = happyGoto action_2
action_1 (16) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (28) = happyShift action_19
action_3 (45) = happyShift action_23
action_3 (63) = happyShift action_21
action_3 _ = happyFail

action_4 (46) = happyShift action_22
action_4 _ = happyFail

action_5 _ = happyReduce_22

action_6 _ = happyReduce_20

action_7 _ = happyReduce_21

action_8 _ = happyReduce_23

action_9 (84) = happyAccept
action_9 _ = happyFail

action_10 _ = happyReduce_2

action_11 _ = happyReduce_3

action_12 _ = happyReduce_4

action_13 _ = happyReduce_5

action_14 (28) = happyShift action_19
action_14 (45) = happyShift action_20
action_14 (63) = happyShift action_21
action_14 _ = happyFail

action_15 (46) = happyShift action_18
action_15 _ = happyFail

action_16 (42) = happyShift action_4
action_16 (47) = happyShift action_5
action_16 (55) = happyShift action_6
action_16 (56) = happyShift action_7
action_16 (57) = happyShift action_8
action_16 (16) = happyGoto action_17
action_16 _ = happyFail

action_17 (28) = happyShift action_19
action_17 (45) = happyShift action_30
action_17 (63) = happyShift action_21
action_17 _ = happyFail

action_18 (32) = happyShift action_28
action_18 (35) = happyShift action_29
action_18 _ = happyReduce_26

action_19 (29) = happyShift action_27
action_19 _ = happyFail

action_20 (30) = happyShift action_25
action_20 (9) = happyGoto action_26
action_20 _ = happyFail

action_21 _ = happyReduce_24

action_22 _ = happyReduce_26

action_23 (30) = happyShift action_25
action_23 (9) = happyGoto action_24
action_23 _ = happyFail

action_24 (35) = happyShift action_37
action_24 _ = happyFail

action_25 (31) = happyShift action_40
action_25 (42) = happyShift action_4
action_25 (47) = happyShift action_5
action_25 (55) = happyShift action_6
action_25 (56) = happyShift action_7
action_25 (57) = happyShift action_8
action_25 (7) = happyGoto action_38
action_25 (16) = happyGoto action_39
action_25 _ = happyFail

action_26 (32) = happyShift action_36
action_26 (35) = happyShift action_37
action_26 (15) = happyGoto action_35
action_26 _ = happyFail

action_27 _ = happyReduce_25

action_28 (42) = happyShift action_4
action_28 (47) = happyShift action_5
action_28 (55) = happyShift action_6
action_28 (56) = happyShift action_7
action_28 (57) = happyShift action_8
action_28 (13) = happyGoto action_32
action_28 (14) = happyGoto action_33
action_28 (16) = happyGoto action_34
action_28 _ = happyReduce_17

action_29 _ = happyReduce_14

action_30 (35) = happyShift action_31
action_30 _ = happyFail

action_31 _ = happyReduce_13

action_32 (42) = happyShift action_4
action_32 (47) = happyShift action_5
action_32 (55) = happyShift action_6
action_32 (56) = happyShift action_7
action_32 (57) = happyShift action_8
action_32 (13) = happyGoto action_32
action_32 (14) = happyGoto action_74
action_32 (16) = happyGoto action_34
action_32 _ = happyReduce_17

action_33 (33) = happyShift action_73
action_33 _ = happyFail

action_34 (28) = happyShift action_19
action_34 (45) = happyShift action_72
action_34 (63) = happyShift action_21
action_34 _ = happyFail

action_35 _ = happyReduce_7

action_36 (30) = happyShift action_54
action_36 (32) = happyShift action_36
action_36 (40) = happyShift action_55
action_36 (41) = happyShift action_56
action_36 (42) = happyShift action_4
action_36 (43) = happyShift action_57
action_36 (44) = happyShift action_58
action_36 (45) = happyShift action_59
action_36 (47) = happyShift action_5
action_36 (48) = happyShift action_60
action_36 (50) = happyShift action_61
action_36 (51) = happyShift action_62
action_36 (52) = happyShift action_63
action_36 (53) = happyShift action_64
action_36 (54) = happyShift action_65
action_36 (55) = happyShift action_6
action_36 (56) = happyShift action_7
action_36 (57) = happyShift action_8
action_36 (59) = happyShift action_66
action_36 (60) = happyShift action_67
action_36 (61) = happyShift action_68
action_36 (63) = happyShift action_69
action_36 (81) = happyShift action_70
action_36 (82) = happyShift action_71
action_36 (15) = happyGoto action_44
action_36 (16) = happyGoto action_45
action_36 (17) = happyGoto action_46
action_36 (18) = happyGoto action_47
action_36 (19) = happyGoto action_48
action_36 (20) = happyGoto action_49
action_36 (22) = happyGoto action_50
action_36 (25) = happyGoto action_51
action_36 (26) = happyGoto action_52
action_36 (27) = happyGoto action_53
action_36 _ = happyReduce_29

action_37 _ = happyReduce_6

action_38 (34) = happyShift action_43
action_38 (8) = happyGoto action_42
action_38 _ = happyReduce_9

action_39 (28) = happyShift action_19
action_39 (45) = happyShift action_41
action_39 (63) = happyShift action_21
action_39 _ = happyFail

action_40 _ = happyReduce_11

action_41 _ = happyReduce_8

action_42 (31) = happyShift action_121
action_42 _ = happyFail

action_43 (42) = happyShift action_4
action_43 (47) = happyShift action_5
action_43 (55) = happyShift action_6
action_43 (56) = happyShift action_7
action_43 (57) = happyShift action_8
action_43 (7) = happyGoto action_120
action_43 (16) = happyGoto action_39
action_43 _ = happyFail

action_44 _ = happyReduce_33

action_45 (28) = happyShift action_19
action_45 (45) = happyShift action_119
action_45 (63) = happyShift action_21
action_45 _ = happyFail

action_46 _ = happyReduce_36

action_47 (33) = happyShift action_118
action_47 _ = happyFail

action_48 (30) = happyShift action_54
action_48 (32) = happyShift action_36
action_48 (40) = happyShift action_55
action_48 (41) = happyShift action_56
action_48 (42) = happyShift action_4
action_48 (43) = happyShift action_57
action_48 (44) = happyShift action_58
action_48 (45) = happyShift action_59
action_48 (47) = happyShift action_5
action_48 (48) = happyShift action_60
action_48 (50) = happyShift action_61
action_48 (51) = happyShift action_62
action_48 (52) = happyShift action_63
action_48 (53) = happyShift action_64
action_48 (54) = happyShift action_65
action_48 (55) = happyShift action_6
action_48 (56) = happyShift action_7
action_48 (57) = happyShift action_8
action_48 (59) = happyShift action_66
action_48 (60) = happyShift action_67
action_48 (61) = happyShift action_68
action_48 (63) = happyShift action_69
action_48 (81) = happyShift action_70
action_48 (82) = happyShift action_71
action_48 (15) = happyGoto action_44
action_48 (16) = happyGoto action_45
action_48 (17) = happyGoto action_46
action_48 (18) = happyGoto action_117
action_48 (19) = happyGoto action_48
action_48 (20) = happyGoto action_49
action_48 (22) = happyGoto action_50
action_48 (25) = happyGoto action_51
action_48 (26) = happyGoto action_52
action_48 (27) = happyGoto action_53
action_48 _ = happyReduce_29

action_49 (35) = happyShift action_116
action_49 _ = happyFail

action_50 _ = happyReduce_32

action_51 (28) = happyShift action_92
action_51 (37) = happyShift action_93
action_51 (38) = happyShift action_94
action_51 (39) = happyShift action_95
action_51 (59) = happyShift action_96
action_51 (62) = happyShift action_97
action_51 (63) = happyShift action_98
action_51 (64) = happyShift action_99
action_51 (65) = happyShift action_100
action_51 (66) = happyShift action_101
action_51 (67) = happyShift action_102
action_51 (68) = happyShift action_103
action_51 (69) = happyShift action_104
action_51 (70) = happyShift action_105
action_51 (71) = happyShift action_106
action_51 (72) = happyShift action_107
action_51 (73) = happyShift action_108
action_51 (74) = happyShift action_109
action_51 (75) = happyShift action_110
action_51 (76) = happyShift action_111
action_51 (77) = happyShift action_112
action_51 (78) = happyShift action_113
action_51 (79) = happyShift action_114
action_51 (80) = happyShift action_115
action_51 _ = happyReduce_37

action_52 _ = happyReduce_57

action_53 _ = happyReduce_52

action_54 (30) = happyShift action_54
action_54 (40) = happyShift action_55
action_54 (41) = happyShift action_56
action_54 (43) = happyShift action_57
action_54 (44) = happyShift action_58
action_54 (45) = happyShift action_59
action_54 (53) = happyShift action_64
action_54 (54) = happyShift action_65
action_54 (59) = happyShift action_66
action_54 (60) = happyShift action_67
action_54 (61) = happyShift action_68
action_54 (63) = happyShift action_69
action_54 (82) = happyShift action_71
action_54 (25) = happyGoto action_91
action_54 (26) = happyGoto action_52
action_54 (27) = happyGoto action_53
action_54 _ = happyFail

action_55 (30) = happyShift action_90
action_55 _ = happyFail

action_56 (30) = happyShift action_89
action_56 _ = happyFail

action_57 _ = happyReduce_87

action_58 _ = happyReduce_88

action_59 (30) = happyShift action_88
action_59 (24) = happyGoto action_87
action_59 _ = happyReduce_55

action_60 (30) = happyShift action_86
action_60 _ = happyFail

action_61 (30) = happyShift action_85
action_61 _ = happyFail

action_62 (30) = happyShift action_84
action_62 _ = happyFail

action_63 (30) = happyShift action_54
action_63 (35) = happyShift action_83
action_63 (40) = happyShift action_55
action_63 (41) = happyShift action_56
action_63 (43) = happyShift action_57
action_63 (44) = happyShift action_58
action_63 (45) = happyShift action_59
action_63 (53) = happyShift action_64
action_63 (54) = happyShift action_65
action_63 (59) = happyShift action_66
action_63 (60) = happyShift action_67
action_63 (61) = happyShift action_68
action_63 (63) = happyShift action_69
action_63 (82) = happyShift action_71
action_63 (25) = happyGoto action_82
action_63 (26) = happyGoto action_52
action_63 (27) = happyGoto action_53
action_63 _ = happyFail

action_64 _ = happyReduce_53

action_65 _ = happyReduce_54

action_66 (30) = happyShift action_54
action_66 (40) = happyShift action_55
action_66 (41) = happyShift action_56
action_66 (43) = happyShift action_57
action_66 (44) = happyShift action_58
action_66 (45) = happyShift action_59
action_66 (53) = happyShift action_64
action_66 (54) = happyShift action_65
action_66 (59) = happyShift action_66
action_66 (60) = happyShift action_67
action_66 (61) = happyShift action_68
action_66 (63) = happyShift action_69
action_66 (82) = happyShift action_71
action_66 (25) = happyGoto action_81
action_66 (26) = happyGoto action_52
action_66 (27) = happyGoto action_53
action_66 _ = happyFail

action_67 (30) = happyShift action_54
action_67 (40) = happyShift action_55
action_67 (41) = happyShift action_56
action_67 (43) = happyShift action_57
action_67 (44) = happyShift action_58
action_67 (45) = happyShift action_59
action_67 (53) = happyShift action_64
action_67 (54) = happyShift action_65
action_67 (59) = happyShift action_66
action_67 (60) = happyShift action_67
action_67 (61) = happyShift action_68
action_67 (63) = happyShift action_69
action_67 (82) = happyShift action_71
action_67 (25) = happyGoto action_80
action_67 (26) = happyGoto action_52
action_67 (27) = happyGoto action_53
action_67 _ = happyFail

action_68 (30) = happyShift action_54
action_68 (40) = happyShift action_55
action_68 (41) = happyShift action_56
action_68 (43) = happyShift action_57
action_68 (44) = happyShift action_58
action_68 (45) = happyShift action_59
action_68 (53) = happyShift action_64
action_68 (54) = happyShift action_65
action_68 (59) = happyShift action_66
action_68 (60) = happyShift action_67
action_68 (61) = happyShift action_68
action_68 (63) = happyShift action_69
action_68 (82) = happyShift action_71
action_68 (25) = happyGoto action_79
action_68 (26) = happyGoto action_52
action_68 (27) = happyGoto action_53
action_68 _ = happyFail

action_69 (30) = happyShift action_54
action_69 (40) = happyShift action_55
action_69 (41) = happyShift action_56
action_69 (43) = happyShift action_57
action_69 (44) = happyShift action_58
action_69 (45) = happyShift action_59
action_69 (53) = happyShift action_64
action_69 (54) = happyShift action_65
action_69 (59) = happyShift action_66
action_69 (60) = happyShift action_67
action_69 (61) = happyShift action_68
action_69 (63) = happyShift action_69
action_69 (82) = happyShift action_71
action_69 (25) = happyGoto action_78
action_69 (26) = happyGoto action_52
action_69 (27) = happyGoto action_53
action_69 _ = happyFail

action_70 (30) = happyShift action_77
action_70 _ = happyFail

action_71 _ = happyReduce_56

action_72 (35) = happyShift action_76
action_72 _ = happyFail

action_73 (35) = happyShift action_75
action_73 _ = happyFail

action_74 _ = happyReduce_18

action_75 _ = happyReduce_15

action_76 _ = happyReduce_16

action_77 (30) = happyShift action_54
action_77 (40) = happyShift action_55
action_77 (41) = happyShift action_56
action_77 (43) = happyShift action_57
action_77 (44) = happyShift action_58
action_77 (45) = happyShift action_59
action_77 (53) = happyShift action_64
action_77 (54) = happyShift action_65
action_77 (59) = happyShift action_66
action_77 (60) = happyShift action_67
action_77 (61) = happyShift action_68
action_77 (63) = happyShift action_69
action_77 (82) = happyShift action_71
action_77 (25) = happyGoto action_157
action_77 (26) = happyGoto action_52
action_77 (27) = happyGoto action_53
action_77 _ = happyFail

action_78 (28) = happyShift action_92
action_78 (38) = happyShift action_94
action_78 (39) = happyShift action_95
action_78 _ = happyReduce_62

action_79 (28) = happyShift action_92
action_79 (38) = happyShift action_94
action_79 (39) = happyShift action_95
action_79 _ = happyReduce_84

action_80 (28) = happyShift action_92
action_80 (38) = happyShift action_94
action_80 (39) = happyShift action_95
action_80 _ = happyReduce_83

action_81 (28) = happyShift action_92
action_81 (38) = happyShift action_94
action_81 (39) = happyShift action_95
action_81 _ = happyReduce_85

action_82 (28) = happyShift action_92
action_82 (35) = happyShift action_156
action_82 (37) = happyShift action_93
action_82 (38) = happyShift action_94
action_82 (39) = happyShift action_95
action_82 (59) = happyShift action_96
action_82 (62) = happyShift action_97
action_82 (63) = happyShift action_98
action_82 (64) = happyShift action_99
action_82 (65) = happyShift action_100
action_82 (66) = happyShift action_101
action_82 (67) = happyShift action_102
action_82 (68) = happyShift action_103
action_82 (69) = happyShift action_104
action_82 (70) = happyShift action_105
action_82 (71) = happyShift action_106
action_82 (72) = happyShift action_107
action_82 (73) = happyShift action_108
action_82 (74) = happyShift action_109
action_82 (75) = happyShift action_110
action_82 (76) = happyShift action_111
action_82 (77) = happyShift action_112
action_82 (78) = happyShift action_113
action_82 _ = happyFail

action_83 _ = happyReduce_45

action_84 (30) = happyShift action_54
action_84 (40) = happyShift action_55
action_84 (41) = happyShift action_56
action_84 (42) = happyShift action_4
action_84 (43) = happyShift action_57
action_84 (44) = happyShift action_58
action_84 (45) = happyShift action_59
action_84 (47) = happyShift action_5
action_84 (53) = happyShift action_64
action_84 (54) = happyShift action_65
action_84 (55) = happyShift action_6
action_84 (56) = happyShift action_7
action_84 (57) = happyShift action_8
action_84 (59) = happyShift action_66
action_84 (60) = happyShift action_67
action_84 (61) = happyShift action_68
action_84 (63) = happyShift action_69
action_84 (82) = happyShift action_71
action_84 (16) = happyGoto action_45
action_84 (17) = happyGoto action_46
action_84 (20) = happyGoto action_154
action_84 (21) = happyGoto action_155
action_84 (25) = happyGoto action_51
action_84 (26) = happyGoto action_52
action_84 (27) = happyGoto action_53
action_84 _ = happyReduce_38

action_85 (30) = happyShift action_54
action_85 (40) = happyShift action_55
action_85 (41) = happyShift action_56
action_85 (43) = happyShift action_57
action_85 (44) = happyShift action_58
action_85 (45) = happyShift action_59
action_85 (53) = happyShift action_64
action_85 (54) = happyShift action_65
action_85 (59) = happyShift action_66
action_85 (60) = happyShift action_67
action_85 (61) = happyShift action_68
action_85 (63) = happyShift action_69
action_85 (82) = happyShift action_71
action_85 (25) = happyGoto action_153
action_85 (26) = happyGoto action_52
action_85 (27) = happyGoto action_53
action_85 _ = happyFail

action_86 (30) = happyShift action_54
action_86 (40) = happyShift action_55
action_86 (41) = happyShift action_56
action_86 (43) = happyShift action_57
action_86 (44) = happyShift action_58
action_86 (45) = happyShift action_59
action_86 (53) = happyShift action_64
action_86 (54) = happyShift action_65
action_86 (59) = happyShift action_66
action_86 (60) = happyShift action_67
action_86 (61) = happyShift action_68
action_86 (63) = happyShift action_69
action_86 (82) = happyShift action_71
action_86 (25) = happyGoto action_152
action_86 (26) = happyGoto action_52
action_86 (27) = happyGoto action_53
action_86 _ = happyFail

action_87 _ = happyReduce_58

action_88 (30) = happyShift action_54
action_88 (31) = happyShift action_151
action_88 (40) = happyShift action_55
action_88 (41) = happyShift action_56
action_88 (43) = happyShift action_57
action_88 (44) = happyShift action_58
action_88 (45) = happyShift action_59
action_88 (53) = happyShift action_64
action_88 (54) = happyShift action_65
action_88 (59) = happyShift action_66
action_88 (60) = happyShift action_67
action_88 (61) = happyShift action_68
action_88 (63) = happyShift action_69
action_88 (82) = happyShift action_71
action_88 (25) = happyGoto action_150
action_88 (26) = happyGoto action_52
action_88 (27) = happyGoto action_53
action_88 _ = happyFail

action_89 (42) = happyShift action_4
action_89 (47) = happyShift action_5
action_89 (55) = happyShift action_6
action_89 (56) = happyShift action_7
action_89 (57) = happyShift action_8
action_89 (16) = happyGoto action_149
action_89 _ = happyFail

action_90 (42) = happyShift action_4
action_90 (47) = happyShift action_5
action_90 (55) = happyShift action_6
action_90 (56) = happyShift action_7
action_90 (57) = happyShift action_8
action_90 (16) = happyGoto action_148
action_90 _ = happyFail

action_91 (28) = happyShift action_92
action_91 (31) = happyShift action_147
action_91 (37) = happyShift action_93
action_91 (38) = happyShift action_94
action_91 (39) = happyShift action_95
action_91 (59) = happyShift action_96
action_91 (62) = happyShift action_97
action_91 (63) = happyShift action_98
action_91 (64) = happyShift action_99
action_91 (65) = happyShift action_100
action_91 (66) = happyShift action_101
action_91 (67) = happyShift action_102
action_91 (68) = happyShift action_103
action_91 (69) = happyShift action_104
action_91 (70) = happyShift action_105
action_91 (71) = happyShift action_106
action_91 (72) = happyShift action_107
action_91 (73) = happyShift action_108
action_91 (74) = happyShift action_109
action_91 (75) = happyShift action_110
action_91 (76) = happyShift action_111
action_91 (77) = happyShift action_112
action_91 (78) = happyShift action_113
action_91 _ = happyFail

action_92 (30) = happyShift action_54
action_92 (40) = happyShift action_55
action_92 (41) = happyShift action_56
action_92 (43) = happyShift action_57
action_92 (44) = happyShift action_58
action_92 (45) = happyShift action_59
action_92 (53) = happyShift action_64
action_92 (54) = happyShift action_65
action_92 (59) = happyShift action_66
action_92 (60) = happyShift action_67
action_92 (61) = happyShift action_68
action_92 (63) = happyShift action_69
action_92 (82) = happyShift action_71
action_92 (25) = happyGoto action_146
action_92 (26) = happyGoto action_52
action_92 (27) = happyGoto action_53
action_92 _ = happyFail

action_93 (30) = happyShift action_54
action_93 (40) = happyShift action_55
action_93 (41) = happyShift action_56
action_93 (43) = happyShift action_57
action_93 (44) = happyShift action_58
action_93 (45) = happyShift action_59
action_93 (53) = happyShift action_64
action_93 (54) = happyShift action_65
action_93 (59) = happyShift action_66
action_93 (60) = happyShift action_67
action_93 (61) = happyShift action_68
action_93 (63) = happyShift action_69
action_93 (82) = happyShift action_71
action_93 (25) = happyGoto action_145
action_93 (26) = happyGoto action_52
action_93 (27) = happyGoto action_53
action_93 _ = happyFail

action_94 (45) = happyShift action_144
action_94 _ = happyFail

action_95 (45) = happyShift action_143
action_95 _ = happyFail

action_96 (30) = happyShift action_54
action_96 (40) = happyShift action_55
action_96 (41) = happyShift action_56
action_96 (43) = happyShift action_57
action_96 (44) = happyShift action_58
action_96 (45) = happyShift action_59
action_96 (53) = happyShift action_64
action_96 (54) = happyShift action_65
action_96 (59) = happyShift action_66
action_96 (60) = happyShift action_67
action_96 (61) = happyShift action_68
action_96 (63) = happyShift action_69
action_96 (82) = happyShift action_71
action_96 (25) = happyGoto action_142
action_96 (26) = happyGoto action_52
action_96 (27) = happyGoto action_53
action_96 _ = happyFail

action_97 (30) = happyShift action_54
action_97 (40) = happyShift action_55
action_97 (41) = happyShift action_56
action_97 (43) = happyShift action_57
action_97 (44) = happyShift action_58
action_97 (45) = happyShift action_59
action_97 (53) = happyShift action_64
action_97 (54) = happyShift action_65
action_97 (59) = happyShift action_66
action_97 (60) = happyShift action_67
action_97 (61) = happyShift action_68
action_97 (63) = happyShift action_69
action_97 (82) = happyShift action_71
action_97 (25) = happyGoto action_141
action_97 (26) = happyGoto action_52
action_97 (27) = happyGoto action_53
action_97 _ = happyFail

action_98 (30) = happyShift action_54
action_98 (40) = happyShift action_55
action_98 (41) = happyShift action_56
action_98 (43) = happyShift action_57
action_98 (44) = happyShift action_58
action_98 (45) = happyShift action_59
action_98 (53) = happyShift action_64
action_98 (54) = happyShift action_65
action_98 (59) = happyShift action_66
action_98 (60) = happyShift action_67
action_98 (61) = happyShift action_68
action_98 (63) = happyShift action_69
action_98 (82) = happyShift action_71
action_98 (25) = happyGoto action_140
action_98 (26) = happyGoto action_52
action_98 (27) = happyGoto action_53
action_98 _ = happyFail

action_99 (30) = happyShift action_54
action_99 (40) = happyShift action_55
action_99 (41) = happyShift action_56
action_99 (43) = happyShift action_57
action_99 (44) = happyShift action_58
action_99 (45) = happyShift action_59
action_99 (53) = happyShift action_64
action_99 (54) = happyShift action_65
action_99 (59) = happyShift action_66
action_99 (60) = happyShift action_67
action_99 (61) = happyShift action_68
action_99 (63) = happyShift action_69
action_99 (82) = happyShift action_71
action_99 (25) = happyGoto action_139
action_99 (26) = happyGoto action_52
action_99 (27) = happyGoto action_53
action_99 _ = happyFail

action_100 (30) = happyShift action_54
action_100 (40) = happyShift action_55
action_100 (41) = happyShift action_56
action_100 (43) = happyShift action_57
action_100 (44) = happyShift action_58
action_100 (45) = happyShift action_59
action_100 (53) = happyShift action_64
action_100 (54) = happyShift action_65
action_100 (59) = happyShift action_66
action_100 (60) = happyShift action_67
action_100 (61) = happyShift action_68
action_100 (63) = happyShift action_69
action_100 (82) = happyShift action_71
action_100 (25) = happyGoto action_138
action_100 (26) = happyGoto action_52
action_100 (27) = happyGoto action_53
action_100 _ = happyFail

action_101 (30) = happyShift action_54
action_101 (40) = happyShift action_55
action_101 (41) = happyShift action_56
action_101 (43) = happyShift action_57
action_101 (44) = happyShift action_58
action_101 (45) = happyShift action_59
action_101 (53) = happyShift action_64
action_101 (54) = happyShift action_65
action_101 (59) = happyShift action_66
action_101 (60) = happyShift action_67
action_101 (61) = happyShift action_68
action_101 (63) = happyShift action_69
action_101 (82) = happyShift action_71
action_101 (25) = happyGoto action_137
action_101 (26) = happyGoto action_52
action_101 (27) = happyGoto action_53
action_101 _ = happyFail

action_102 (30) = happyShift action_54
action_102 (40) = happyShift action_55
action_102 (41) = happyShift action_56
action_102 (43) = happyShift action_57
action_102 (44) = happyShift action_58
action_102 (45) = happyShift action_59
action_102 (53) = happyShift action_64
action_102 (54) = happyShift action_65
action_102 (59) = happyShift action_66
action_102 (60) = happyShift action_67
action_102 (61) = happyShift action_68
action_102 (63) = happyShift action_69
action_102 (82) = happyShift action_71
action_102 (25) = happyGoto action_136
action_102 (26) = happyGoto action_52
action_102 (27) = happyGoto action_53
action_102 _ = happyFail

action_103 (30) = happyShift action_54
action_103 (40) = happyShift action_55
action_103 (41) = happyShift action_56
action_103 (43) = happyShift action_57
action_103 (44) = happyShift action_58
action_103 (45) = happyShift action_59
action_103 (53) = happyShift action_64
action_103 (54) = happyShift action_65
action_103 (59) = happyShift action_66
action_103 (60) = happyShift action_67
action_103 (61) = happyShift action_68
action_103 (63) = happyShift action_69
action_103 (82) = happyShift action_71
action_103 (25) = happyGoto action_135
action_103 (26) = happyGoto action_52
action_103 (27) = happyGoto action_53
action_103 _ = happyFail

action_104 (30) = happyShift action_54
action_104 (40) = happyShift action_55
action_104 (41) = happyShift action_56
action_104 (43) = happyShift action_57
action_104 (44) = happyShift action_58
action_104 (45) = happyShift action_59
action_104 (53) = happyShift action_64
action_104 (54) = happyShift action_65
action_104 (59) = happyShift action_66
action_104 (60) = happyShift action_67
action_104 (61) = happyShift action_68
action_104 (63) = happyShift action_69
action_104 (82) = happyShift action_71
action_104 (25) = happyGoto action_134
action_104 (26) = happyGoto action_52
action_104 (27) = happyGoto action_53
action_104 _ = happyFail

action_105 (30) = happyShift action_54
action_105 (40) = happyShift action_55
action_105 (41) = happyShift action_56
action_105 (43) = happyShift action_57
action_105 (44) = happyShift action_58
action_105 (45) = happyShift action_59
action_105 (53) = happyShift action_64
action_105 (54) = happyShift action_65
action_105 (59) = happyShift action_66
action_105 (60) = happyShift action_67
action_105 (61) = happyShift action_68
action_105 (63) = happyShift action_69
action_105 (82) = happyShift action_71
action_105 (25) = happyGoto action_133
action_105 (26) = happyGoto action_52
action_105 (27) = happyGoto action_53
action_105 _ = happyFail

action_106 (30) = happyShift action_54
action_106 (40) = happyShift action_55
action_106 (41) = happyShift action_56
action_106 (43) = happyShift action_57
action_106 (44) = happyShift action_58
action_106 (45) = happyShift action_59
action_106 (53) = happyShift action_64
action_106 (54) = happyShift action_65
action_106 (59) = happyShift action_66
action_106 (60) = happyShift action_67
action_106 (61) = happyShift action_68
action_106 (63) = happyShift action_69
action_106 (82) = happyShift action_71
action_106 (25) = happyGoto action_132
action_106 (26) = happyGoto action_52
action_106 (27) = happyGoto action_53
action_106 _ = happyFail

action_107 (30) = happyShift action_54
action_107 (40) = happyShift action_55
action_107 (41) = happyShift action_56
action_107 (43) = happyShift action_57
action_107 (44) = happyShift action_58
action_107 (45) = happyShift action_59
action_107 (53) = happyShift action_64
action_107 (54) = happyShift action_65
action_107 (59) = happyShift action_66
action_107 (60) = happyShift action_67
action_107 (61) = happyShift action_68
action_107 (63) = happyShift action_69
action_107 (82) = happyShift action_71
action_107 (25) = happyGoto action_131
action_107 (26) = happyGoto action_52
action_107 (27) = happyGoto action_53
action_107 _ = happyFail

action_108 (30) = happyShift action_54
action_108 (40) = happyShift action_55
action_108 (41) = happyShift action_56
action_108 (43) = happyShift action_57
action_108 (44) = happyShift action_58
action_108 (45) = happyShift action_59
action_108 (53) = happyShift action_64
action_108 (54) = happyShift action_65
action_108 (59) = happyShift action_66
action_108 (60) = happyShift action_67
action_108 (61) = happyShift action_68
action_108 (63) = happyShift action_69
action_108 (82) = happyShift action_71
action_108 (25) = happyGoto action_130
action_108 (26) = happyGoto action_52
action_108 (27) = happyGoto action_53
action_108 _ = happyFail

action_109 (30) = happyShift action_54
action_109 (40) = happyShift action_55
action_109 (41) = happyShift action_56
action_109 (43) = happyShift action_57
action_109 (44) = happyShift action_58
action_109 (45) = happyShift action_59
action_109 (53) = happyShift action_64
action_109 (54) = happyShift action_65
action_109 (59) = happyShift action_66
action_109 (60) = happyShift action_67
action_109 (61) = happyShift action_68
action_109 (63) = happyShift action_69
action_109 (82) = happyShift action_71
action_109 (25) = happyGoto action_129
action_109 (26) = happyGoto action_52
action_109 (27) = happyGoto action_53
action_109 _ = happyFail

action_110 (30) = happyShift action_54
action_110 (40) = happyShift action_55
action_110 (41) = happyShift action_56
action_110 (43) = happyShift action_57
action_110 (44) = happyShift action_58
action_110 (45) = happyShift action_59
action_110 (53) = happyShift action_64
action_110 (54) = happyShift action_65
action_110 (59) = happyShift action_66
action_110 (60) = happyShift action_67
action_110 (61) = happyShift action_68
action_110 (63) = happyShift action_69
action_110 (82) = happyShift action_71
action_110 (25) = happyGoto action_128
action_110 (26) = happyGoto action_52
action_110 (27) = happyGoto action_53
action_110 _ = happyFail

action_111 (30) = happyShift action_54
action_111 (40) = happyShift action_55
action_111 (41) = happyShift action_56
action_111 (43) = happyShift action_57
action_111 (44) = happyShift action_58
action_111 (45) = happyShift action_59
action_111 (53) = happyShift action_64
action_111 (54) = happyShift action_65
action_111 (59) = happyShift action_66
action_111 (60) = happyShift action_67
action_111 (61) = happyShift action_68
action_111 (63) = happyShift action_69
action_111 (82) = happyShift action_71
action_111 (25) = happyGoto action_127
action_111 (26) = happyGoto action_52
action_111 (27) = happyGoto action_53
action_111 _ = happyFail

action_112 (30) = happyShift action_54
action_112 (40) = happyShift action_55
action_112 (41) = happyShift action_56
action_112 (43) = happyShift action_57
action_112 (44) = happyShift action_58
action_112 (45) = happyShift action_59
action_112 (53) = happyShift action_64
action_112 (54) = happyShift action_65
action_112 (59) = happyShift action_66
action_112 (60) = happyShift action_67
action_112 (61) = happyShift action_68
action_112 (63) = happyShift action_69
action_112 (82) = happyShift action_71
action_112 (25) = happyGoto action_126
action_112 (26) = happyGoto action_52
action_112 (27) = happyGoto action_53
action_112 _ = happyFail

action_113 (30) = happyShift action_54
action_113 (40) = happyShift action_55
action_113 (41) = happyShift action_56
action_113 (43) = happyShift action_57
action_113 (44) = happyShift action_58
action_113 (45) = happyShift action_59
action_113 (53) = happyShift action_64
action_113 (54) = happyShift action_65
action_113 (59) = happyShift action_66
action_113 (60) = happyShift action_67
action_113 (61) = happyShift action_68
action_113 (63) = happyShift action_69
action_113 (82) = happyShift action_71
action_113 (25) = happyGoto action_125
action_113 (26) = happyGoto action_52
action_113 (27) = happyGoto action_53
action_113 _ = happyFail

action_114 (30) = happyShift action_54
action_114 (40) = happyShift action_55
action_114 (41) = happyShift action_56
action_114 (43) = happyShift action_57
action_114 (44) = happyShift action_58
action_114 (45) = happyShift action_59
action_114 (53) = happyShift action_64
action_114 (54) = happyShift action_65
action_114 (59) = happyShift action_66
action_114 (60) = happyShift action_67
action_114 (61) = happyShift action_68
action_114 (63) = happyShift action_69
action_114 (82) = happyShift action_71
action_114 (25) = happyGoto action_124
action_114 (26) = happyGoto action_52
action_114 (27) = happyGoto action_53
action_114 _ = happyFail

action_115 _ = happyReduce_35

action_116 _ = happyReduce_31

action_117 _ = happyReduce_30

action_118 _ = happyReduce_19

action_119 (79) = happyShift action_123
action_119 _ = happyReduce_28

action_120 (34) = happyShift action_43
action_120 (8) = happyGoto action_122
action_120 _ = happyReduce_9

action_121 _ = happyReduce_12

action_122 _ = happyReduce_10

action_123 (30) = happyShift action_54
action_123 (40) = happyShift action_55
action_123 (41) = happyShift action_56
action_123 (43) = happyShift action_57
action_123 (44) = happyShift action_58
action_123 (45) = happyShift action_59
action_123 (53) = happyShift action_64
action_123 (54) = happyShift action_65
action_123 (59) = happyShift action_66
action_123 (60) = happyShift action_67
action_123 (61) = happyShift action_68
action_123 (63) = happyShift action_69
action_123 (82) = happyShift action_71
action_123 (25) = happyGoto action_168
action_123 (26) = happyGoto action_52
action_123 (27) = happyGoto action_53
action_123 _ = happyFail

action_124 (28) = happyShift action_92
action_124 (37) = happyShift action_93
action_124 (38) = happyShift action_94
action_124 (39) = happyShift action_95
action_124 (59) = happyShift action_96
action_124 (62) = happyShift action_97
action_124 (63) = happyShift action_98
action_124 (64) = happyShift action_99
action_124 (65) = happyShift action_100
action_124 (66) = happyShift action_101
action_124 (67) = happyShift action_102
action_124 (68) = happyShift action_103
action_124 (69) = happyShift action_104
action_124 (70) = happyShift action_105
action_124 (71) = happyShift action_106
action_124 (72) = happyShift action_107
action_124 (73) = happyShift action_108
action_124 (74) = happyShift action_109
action_124 (75) = happyShift action_110
action_124 (76) = happyShift action_111
action_124 (77) = happyShift action_112
action_124 (78) = happyShift action_113
action_124 _ = happyReduce_34

action_125 (28) = happyShift action_92
action_125 (38) = happyShift action_94
action_125 (39) = happyShift action_95
action_125 (59) = happyShift action_96
action_125 (62) = happyShift action_97
action_125 (63) = happyShift action_98
action_125 (64) = happyShift action_99
action_125 (65) = happyShift action_100
action_125 (66) = happyShift action_101
action_125 (67) = happyShift action_102
action_125 (68) = happyShift action_103
action_125 (69) = happyShift action_104
action_125 (70) = happyShift action_105
action_125 (71) = happyShift action_106
action_125 _ = happyReduce_82

action_126 (28) = happyShift action_92
action_126 (38) = happyShift action_94
action_126 (39) = happyShift action_95
action_126 (59) = happyShift action_96
action_126 (62) = happyShift action_97
action_126 (63) = happyShift action_98
action_126 (64) = happyShift action_99
action_126 (65) = happyShift action_100
action_126 (66) = happyShift action_101
action_126 (67) = happyShift action_102
action_126 (68) = happyShift action_103
action_126 (69) = happyShift action_104
action_126 (70) = happyShift action_105
action_126 (71) = happyShift action_106
action_126 (72) = happyShift action_107
action_126 (73) = happyShift action_108
action_126 (74) = happyShift action_109
action_126 (75) = happyShift action_110
action_126 (76) = happyShift action_111
action_126 (78) = happyShift action_113
action_126 _ = happyReduce_81

action_127 (28) = happyShift action_92
action_127 (38) = happyShift action_94
action_127 (39) = happyShift action_95
action_127 (59) = happyShift action_96
action_127 (62) = happyShift action_97
action_127 (63) = happyShift action_98
action_127 (64) = happyShift action_99
action_127 (65) = happyShift action_100
action_127 (66) = happyShift action_101
action_127 (67) = happyShift action_102
action_127 (68) = happyShift action_103
action_127 (69) = happyShift action_104
action_127 (70) = happyShift action_105
action_127 (71) = happyShift action_106
action_127 (72) = happyShift action_107
action_127 (73) = happyShift action_108
action_127 (74) = happyShift action_109
action_127 (75) = happyShift action_110
action_127 (78) = happyShift action_113
action_127 _ = happyReduce_80

action_128 (28) = happyShift action_92
action_128 (38) = happyShift action_94
action_128 (39) = happyShift action_95
action_128 (59) = happyShift action_96
action_128 (62) = happyShift action_97
action_128 (63) = happyShift action_98
action_128 (64) = happyShift action_99
action_128 (65) = happyShift action_100
action_128 (66) = happyShift action_101
action_128 (67) = happyShift action_102
action_128 (68) = happyShift action_103
action_128 (69) = happyShift action_104
action_128 (70) = happyShift action_105
action_128 (71) = happyShift action_106
action_128 (72) = happyShift action_107
action_128 (73) = happyShift action_108
action_128 (74) = happyShift action_109
action_128 (78) = happyShift action_113
action_128 _ = happyReduce_79

action_129 (28) = happyShift action_92
action_129 (38) = happyShift action_94
action_129 (39) = happyShift action_95
action_129 (59) = happyShift action_96
action_129 (62) = happyShift action_97
action_129 (63) = happyShift action_98
action_129 (64) = happyShift action_99
action_129 (65) = happyShift action_100
action_129 (66) = happyShift action_101
action_129 (67) = happyShift action_102
action_129 (68) = happyShift action_103
action_129 (69) = happyShift action_104
action_129 (70) = happyShift action_105
action_129 (71) = happyShift action_106
action_129 (72) = happyShift action_107
action_129 (73) = happyShift action_108
action_129 (78) = happyShift action_113
action_129 _ = happyReduce_78

action_130 (28) = happyShift action_92
action_130 (38) = happyShift action_94
action_130 (39) = happyShift action_95
action_130 (59) = happyShift action_96
action_130 (62) = happyShift action_97
action_130 (63) = happyShift action_98
action_130 (64) = happyShift action_99
action_130 (65) = happyShift action_100
action_130 (66) = happyShift action_101
action_130 (67) = happyShift action_102
action_130 (68) = happyShift action_103
action_130 (69) = happyShift action_104
action_130 (70) = happyShift action_105
action_130 (71) = happyShift action_106
action_130 (72) = happyShift action_107
action_130 (78) = happyShift action_113
action_130 _ = happyReduce_77

action_131 (28) = happyShift action_92
action_131 (38) = happyShift action_94
action_131 (39) = happyShift action_95
action_131 (59) = happyShift action_96
action_131 (62) = happyShift action_97
action_131 (63) = happyShift action_98
action_131 (64) = happyShift action_99
action_131 (65) = happyShift action_100
action_131 (66) = happyShift action_101
action_131 (67) = happyShift action_102
action_131 (68) = happyShift action_103
action_131 (69) = happyShift action_104
action_131 (70) = happyShift action_105
action_131 (71) = happyShift action_106
action_131 _ = happyReduce_76

action_132 (28) = happyShift action_92
action_132 (38) = happyShift action_94
action_132 (39) = happyShift action_95
action_132 (59) = happyShift action_96
action_132 (62) = happyShift action_97
action_132 (63) = happyShift action_98
action_132 (64) = happyShift action_99
action_132 (65) = happyShift action_100
action_132 (66) = happyShift action_101
action_132 (67) = happyShift action_102
action_132 _ = happyReduce_74

action_133 (28) = happyShift action_92
action_133 (38) = happyShift action_94
action_133 (39) = happyShift action_95
action_133 (59) = happyShift action_96
action_133 (62) = happyShift action_97
action_133 (63) = happyShift action_98
action_133 (64) = happyShift action_99
action_133 (65) = happyShift action_100
action_133 (66) = happyShift action_101
action_133 (67) = happyShift action_102
action_133 _ = happyReduce_75

action_134 (28) = happyShift action_92
action_134 (38) = happyShift action_94
action_134 (39) = happyShift action_95
action_134 (59) = happyShift action_96
action_134 (62) = happyShift action_97
action_134 (63) = happyShift action_98
action_134 (64) = happyShift action_99
action_134 (65) = happyShift action_100
action_134 (66) = happyShift action_101
action_134 (67) = happyShift action_102
action_134 _ = happyReduce_73

action_135 (28) = happyShift action_92
action_135 (38) = happyShift action_94
action_135 (39) = happyShift action_95
action_135 (59) = happyShift action_96
action_135 (62) = happyShift action_97
action_135 (63) = happyShift action_98
action_135 (64) = happyShift action_99
action_135 (65) = happyShift action_100
action_135 (66) = happyShift action_101
action_135 (67) = happyShift action_102
action_135 _ = happyReduce_72

action_136 (28) = happyShift action_92
action_136 (38) = happyShift action_94
action_136 (39) = happyShift action_95
action_136 (59) = happyShift action_96
action_136 (62) = happyShift action_97
action_136 (63) = happyShift action_98
action_136 (64) = happyShift action_99
action_136 (65) = happyShift action_100
action_136 _ = happyReduce_71

action_137 (28) = happyShift action_92
action_137 (38) = happyShift action_94
action_137 (39) = happyShift action_95
action_137 (59) = happyShift action_96
action_137 (62) = happyShift action_97
action_137 (63) = happyShift action_98
action_137 (64) = happyShift action_99
action_137 (65) = happyShift action_100
action_137 _ = happyReduce_70

action_138 (28) = happyShift action_92
action_138 (38) = happyShift action_94
action_138 (39) = happyShift action_95
action_138 _ = happyReduce_69

action_139 (28) = happyShift action_92
action_139 (38) = happyShift action_94
action_139 (39) = happyShift action_95
action_139 _ = happyReduce_68

action_140 (28) = happyShift action_92
action_140 (38) = happyShift action_94
action_140 (39) = happyShift action_95
action_140 _ = happyReduce_67

action_141 (28) = happyShift action_92
action_141 (38) = happyShift action_94
action_141 (39) = happyShift action_95
action_141 (63) = happyShift action_98
action_141 (64) = happyShift action_99
action_141 (65) = happyShift action_100
action_141 _ = happyReduce_66

action_142 (28) = happyShift action_92
action_142 (38) = happyShift action_94
action_142 (39) = happyShift action_95
action_142 (63) = happyShift action_98
action_142 (64) = happyShift action_99
action_142 (65) = happyShift action_100
action_142 _ = happyReduce_65

action_143 _ = happyReduce_60

action_144 _ = happyReduce_59

action_145 (28) = happyShift action_92
action_145 (36) = happyShift action_167
action_145 (37) = happyShift action_93
action_145 (38) = happyShift action_94
action_145 (39) = happyShift action_95
action_145 (59) = happyShift action_96
action_145 (62) = happyShift action_97
action_145 (63) = happyShift action_98
action_145 (64) = happyShift action_99
action_145 (65) = happyShift action_100
action_145 (66) = happyShift action_101
action_145 (67) = happyShift action_102
action_145 (68) = happyShift action_103
action_145 (69) = happyShift action_104
action_145 (70) = happyShift action_105
action_145 (71) = happyShift action_106
action_145 (72) = happyShift action_107
action_145 (73) = happyShift action_108
action_145 (74) = happyShift action_109
action_145 (75) = happyShift action_110
action_145 (76) = happyShift action_111
action_145 (77) = happyShift action_112
action_145 (78) = happyShift action_113
action_145 _ = happyFail

action_146 (28) = happyShift action_92
action_146 (29) = happyShift action_166
action_146 (37) = happyShift action_93
action_146 (38) = happyShift action_94
action_146 (39) = happyShift action_95
action_146 (59) = happyShift action_96
action_146 (62) = happyShift action_97
action_146 (63) = happyShift action_98
action_146 (64) = happyShift action_99
action_146 (65) = happyShift action_100
action_146 (66) = happyShift action_101
action_146 (67) = happyShift action_102
action_146 (68) = happyShift action_103
action_146 (69) = happyShift action_104
action_146 (70) = happyShift action_105
action_146 (71) = happyShift action_106
action_146 (72) = happyShift action_107
action_146 (73) = happyShift action_108
action_146 (74) = happyShift action_109
action_146 (75) = happyShift action_110
action_146 (76) = happyShift action_111
action_146 (77) = happyShift action_112
action_146 (78) = happyShift action_113
action_146 _ = happyFail

action_147 _ = happyReduce_51

action_148 (28) = happyShift action_19
action_148 (31) = happyShift action_165
action_148 (63) = happyShift action_21
action_148 _ = happyFail

action_149 (28) = happyShift action_19
action_149 (34) = happyShift action_164
action_149 (63) = happyShift action_21
action_149 _ = happyFail

action_150 (28) = happyShift action_92
action_150 (34) = happyShift action_163
action_150 (37) = happyShift action_93
action_150 (38) = happyShift action_94
action_150 (39) = happyShift action_95
action_150 (59) = happyShift action_96
action_150 (62) = happyShift action_97
action_150 (63) = happyShift action_98
action_150 (64) = happyShift action_99
action_150 (65) = happyShift action_100
action_150 (66) = happyShift action_101
action_150 (67) = happyShift action_102
action_150 (68) = happyShift action_103
action_150 (69) = happyShift action_104
action_150 (70) = happyShift action_105
action_150 (71) = happyShift action_106
action_150 (72) = happyShift action_107
action_150 (73) = happyShift action_108
action_150 (74) = happyShift action_109
action_150 (75) = happyShift action_110
action_150 (76) = happyShift action_111
action_150 (77) = happyShift action_112
action_150 (78) = happyShift action_113
action_150 (23) = happyGoto action_162
action_150 _ = happyReduce_47

action_151 _ = happyReduce_49

action_152 (28) = happyShift action_92
action_152 (31) = happyShift action_161
action_152 (37) = happyShift action_93
action_152 (38) = happyShift action_94
action_152 (39) = happyShift action_95
action_152 (59) = happyShift action_96
action_152 (62) = happyShift action_97
action_152 (63) = happyShift action_98
action_152 (64) = happyShift action_99
action_152 (65) = happyShift action_100
action_152 (66) = happyShift action_101
action_152 (67) = happyShift action_102
action_152 (68) = happyShift action_103
action_152 (69) = happyShift action_104
action_152 (70) = happyShift action_105
action_152 (71) = happyShift action_106
action_152 (72) = happyShift action_107
action_152 (73) = happyShift action_108
action_152 (74) = happyShift action_109
action_152 (75) = happyShift action_110
action_152 (76) = happyShift action_111
action_152 (77) = happyShift action_112
action_152 (78) = happyShift action_113
action_152 _ = happyFail

action_153 (28) = happyShift action_92
action_153 (31) = happyShift action_160
action_153 (37) = happyShift action_93
action_153 (38) = happyShift action_94
action_153 (39) = happyShift action_95
action_153 (59) = happyShift action_96
action_153 (62) = happyShift action_97
action_153 (63) = happyShift action_98
action_153 (64) = happyShift action_99
action_153 (65) = happyShift action_100
action_153 (66) = happyShift action_101
action_153 (67) = happyShift action_102
action_153 (68) = happyShift action_103
action_153 (69) = happyShift action_104
action_153 (70) = happyShift action_105
action_153 (71) = happyShift action_106
action_153 (72) = happyShift action_107
action_153 (73) = happyShift action_108
action_153 (74) = happyShift action_109
action_153 (75) = happyShift action_110
action_153 (76) = happyShift action_111
action_153 (77) = happyShift action_112
action_153 (78) = happyShift action_113
action_153 _ = happyFail

action_154 _ = happyReduce_39

action_155 (35) = happyShift action_159
action_155 _ = happyFail

action_156 _ = happyReduce_44

action_157 (28) = happyShift action_92
action_157 (31) = happyShift action_158
action_157 (37) = happyShift action_93
action_157 (38) = happyShift action_94
action_157 (39) = happyShift action_95
action_157 (59) = happyShift action_96
action_157 (62) = happyShift action_97
action_157 (63) = happyShift action_98
action_157 (64) = happyShift action_99
action_157 (65) = happyShift action_100
action_157 (66) = happyShift action_101
action_157 (67) = happyShift action_102
action_157 (68) = happyShift action_103
action_157 (69) = happyShift action_104
action_157 (70) = happyShift action_105
action_157 (71) = happyShift action_106
action_157 (72) = happyShift action_107
action_157 (73) = happyShift action_108
action_157 (74) = happyShift action_109
action_157 (75) = happyShift action_110
action_157 (76) = happyShift action_111
action_157 (77) = happyShift action_112
action_157 (78) = happyShift action_113
action_157 _ = happyFail

action_158 (35) = happyShift action_176
action_158 _ = happyFail

action_159 (30) = happyShift action_54
action_159 (40) = happyShift action_55
action_159 (41) = happyShift action_56
action_159 (43) = happyShift action_57
action_159 (44) = happyShift action_58
action_159 (45) = happyShift action_59
action_159 (53) = happyShift action_64
action_159 (54) = happyShift action_65
action_159 (59) = happyShift action_66
action_159 (60) = happyShift action_67
action_159 (61) = happyShift action_68
action_159 (63) = happyShift action_69
action_159 (82) = happyShift action_71
action_159 (25) = happyGoto action_175
action_159 (26) = happyGoto action_52
action_159 (27) = happyGoto action_53
action_159 _ = happyFail

action_160 (30) = happyShift action_54
action_160 (32) = happyShift action_36
action_160 (40) = happyShift action_55
action_160 (41) = happyShift action_56
action_160 (42) = happyShift action_4
action_160 (43) = happyShift action_57
action_160 (44) = happyShift action_58
action_160 (45) = happyShift action_59
action_160 (47) = happyShift action_5
action_160 (48) = happyShift action_60
action_160 (50) = happyShift action_61
action_160 (51) = happyShift action_62
action_160 (52) = happyShift action_63
action_160 (53) = happyShift action_64
action_160 (54) = happyShift action_65
action_160 (55) = happyShift action_6
action_160 (56) = happyShift action_7
action_160 (57) = happyShift action_8
action_160 (59) = happyShift action_66
action_160 (60) = happyShift action_67
action_160 (61) = happyShift action_68
action_160 (63) = happyShift action_69
action_160 (81) = happyShift action_70
action_160 (82) = happyShift action_71
action_160 (15) = happyGoto action_44
action_160 (16) = happyGoto action_45
action_160 (17) = happyGoto action_46
action_160 (19) = happyGoto action_174
action_160 (20) = happyGoto action_49
action_160 (22) = happyGoto action_50
action_160 (25) = happyGoto action_51
action_160 (26) = happyGoto action_52
action_160 (27) = happyGoto action_53
action_160 _ = happyFail

action_161 (30) = happyShift action_54
action_161 (32) = happyShift action_36
action_161 (40) = happyShift action_55
action_161 (41) = happyShift action_56
action_161 (42) = happyShift action_4
action_161 (43) = happyShift action_57
action_161 (44) = happyShift action_58
action_161 (45) = happyShift action_59
action_161 (47) = happyShift action_5
action_161 (48) = happyShift action_60
action_161 (50) = happyShift action_61
action_161 (51) = happyShift action_62
action_161 (52) = happyShift action_63
action_161 (53) = happyShift action_64
action_161 (54) = happyShift action_65
action_161 (55) = happyShift action_6
action_161 (56) = happyShift action_7
action_161 (57) = happyShift action_8
action_161 (59) = happyShift action_66
action_161 (60) = happyShift action_67
action_161 (61) = happyShift action_68
action_161 (63) = happyShift action_69
action_161 (81) = happyShift action_70
action_161 (82) = happyShift action_71
action_161 (15) = happyGoto action_44
action_161 (16) = happyGoto action_45
action_161 (17) = happyGoto action_46
action_161 (19) = happyGoto action_173
action_161 (20) = happyGoto action_49
action_161 (22) = happyGoto action_50
action_161 (25) = happyGoto action_51
action_161 (26) = happyGoto action_52
action_161 (27) = happyGoto action_53
action_161 _ = happyFail

action_162 (31) = happyShift action_172
action_162 _ = happyFail

action_163 (30) = happyShift action_54
action_163 (40) = happyShift action_55
action_163 (41) = happyShift action_56
action_163 (43) = happyShift action_57
action_163 (44) = happyShift action_58
action_163 (45) = happyShift action_59
action_163 (53) = happyShift action_64
action_163 (54) = happyShift action_65
action_163 (59) = happyShift action_66
action_163 (60) = happyShift action_67
action_163 (61) = happyShift action_68
action_163 (63) = happyShift action_69
action_163 (82) = happyShift action_71
action_163 (25) = happyGoto action_171
action_163 (26) = happyGoto action_52
action_163 (27) = happyGoto action_53
action_163 _ = happyFail

action_164 (30) = happyShift action_54
action_164 (40) = happyShift action_55
action_164 (41) = happyShift action_56
action_164 (43) = happyShift action_57
action_164 (44) = happyShift action_58
action_164 (45) = happyShift action_59
action_164 (53) = happyShift action_64
action_164 (54) = happyShift action_65
action_164 (59) = happyShift action_66
action_164 (60) = happyShift action_67
action_164 (61) = happyShift action_68
action_164 (63) = happyShift action_69
action_164 (82) = happyShift action_71
action_164 (25) = happyGoto action_170
action_164 (26) = happyGoto action_52
action_164 (27) = happyGoto action_53
action_164 _ = happyFail

action_165 _ = happyReduce_61

action_166 _ = happyReduce_64

action_167 (30) = happyShift action_54
action_167 (40) = happyShift action_55
action_167 (41) = happyShift action_56
action_167 (43) = happyShift action_57
action_167 (44) = happyShift action_58
action_167 (45) = happyShift action_59
action_167 (53) = happyShift action_64
action_167 (54) = happyShift action_65
action_167 (59) = happyShift action_66
action_167 (60) = happyShift action_67
action_167 (61) = happyShift action_68
action_167 (63) = happyShift action_69
action_167 (82) = happyShift action_71
action_167 (25) = happyGoto action_169
action_167 (26) = happyGoto action_52
action_167 (27) = happyGoto action_53
action_167 _ = happyFail

action_168 (28) = happyShift action_92
action_168 (37) = happyShift action_93
action_168 (38) = happyShift action_94
action_168 (39) = happyShift action_95
action_168 (59) = happyShift action_96
action_168 (62) = happyShift action_97
action_168 (63) = happyShift action_98
action_168 (64) = happyShift action_99
action_168 (65) = happyShift action_100
action_168 (66) = happyShift action_101
action_168 (67) = happyShift action_102
action_168 (68) = happyShift action_103
action_168 (69) = happyShift action_104
action_168 (70) = happyShift action_105
action_168 (71) = happyShift action_106
action_168 (72) = happyShift action_107
action_168 (73) = happyShift action_108
action_168 (74) = happyShift action_109
action_168 (75) = happyShift action_110
action_168 (76) = happyShift action_111
action_168 (77) = happyShift action_112
action_168 (78) = happyShift action_113
action_168 _ = happyReduce_27

action_169 (28) = happyShift action_92
action_169 (37) = happyShift action_93
action_169 (38) = happyShift action_94
action_169 (39) = happyShift action_95
action_169 (59) = happyShift action_96
action_169 (62) = happyShift action_97
action_169 (63) = happyShift action_98
action_169 (64) = happyShift action_99
action_169 (65) = happyShift action_100
action_169 (66) = happyShift action_101
action_169 (67) = happyShift action_102
action_169 (68) = happyShift action_103
action_169 (69) = happyShift action_104
action_169 (70) = happyShift action_105
action_169 (71) = happyShift action_106
action_169 (72) = happyShift action_107
action_169 (73) = happyShift action_108
action_169 (74) = happyShift action_109
action_169 (75) = happyShift action_110
action_169 (76) = happyShift action_111
action_169 (77) = happyShift action_112
action_169 (78) = happyShift action_113
action_169 _ = happyReduce_86

action_170 (28) = happyShift action_92
action_170 (31) = happyShift action_180
action_170 (37) = happyShift action_93
action_170 (38) = happyShift action_94
action_170 (39) = happyShift action_95
action_170 (59) = happyShift action_96
action_170 (62) = happyShift action_97
action_170 (63) = happyShift action_98
action_170 (64) = happyShift action_99
action_170 (65) = happyShift action_100
action_170 (66) = happyShift action_101
action_170 (67) = happyShift action_102
action_170 (68) = happyShift action_103
action_170 (69) = happyShift action_104
action_170 (70) = happyShift action_105
action_170 (71) = happyShift action_106
action_170 (72) = happyShift action_107
action_170 (73) = happyShift action_108
action_170 (74) = happyShift action_109
action_170 (75) = happyShift action_110
action_170 (76) = happyShift action_111
action_170 (77) = happyShift action_112
action_170 (78) = happyShift action_113
action_170 _ = happyFail

action_171 (28) = happyShift action_92
action_171 (34) = happyShift action_163
action_171 (37) = happyShift action_93
action_171 (38) = happyShift action_94
action_171 (39) = happyShift action_95
action_171 (59) = happyShift action_96
action_171 (62) = happyShift action_97
action_171 (63) = happyShift action_98
action_171 (64) = happyShift action_99
action_171 (65) = happyShift action_100
action_171 (66) = happyShift action_101
action_171 (67) = happyShift action_102
action_171 (68) = happyShift action_103
action_171 (69) = happyShift action_104
action_171 (70) = happyShift action_105
action_171 (71) = happyShift action_106
action_171 (72) = happyShift action_107
action_171 (73) = happyShift action_108
action_171 (74) = happyShift action_109
action_171 (75) = happyShift action_110
action_171 (76) = happyShift action_111
action_171 (77) = happyShift action_112
action_171 (78) = happyShift action_113
action_171 (23) = happyGoto action_179
action_171 _ = happyReduce_47

action_172 _ = happyReduce_50

action_173 (49) = happyShift action_178
action_173 _ = happyReduce_40

action_174 _ = happyReduce_42

action_175 (28) = happyShift action_92
action_175 (35) = happyShift action_177
action_175 (37) = happyShift action_93
action_175 (38) = happyShift action_94
action_175 (39) = happyShift action_95
action_175 (59) = happyShift action_96
action_175 (62) = happyShift action_97
action_175 (63) = happyShift action_98
action_175 (64) = happyShift action_99
action_175 (65) = happyShift action_100
action_175 (66) = happyShift action_101
action_175 (67) = happyShift action_102
action_175 (68) = happyShift action_103
action_175 (69) = happyShift action_104
action_175 (70) = happyShift action_105
action_175 (71) = happyShift action_106
action_175 (72) = happyShift action_107
action_175 (73) = happyShift action_108
action_175 (74) = happyShift action_109
action_175 (75) = happyShift action_110
action_175 (76) = happyShift action_111
action_175 (77) = happyShift action_112
action_175 (78) = happyShift action_113
action_175 _ = happyFail

action_176 _ = happyReduce_46

action_177 (30) = happyShift action_54
action_177 (40) = happyShift action_55
action_177 (41) = happyShift action_56
action_177 (42) = happyShift action_4
action_177 (43) = happyShift action_57
action_177 (44) = happyShift action_58
action_177 (45) = happyShift action_59
action_177 (47) = happyShift action_5
action_177 (53) = happyShift action_64
action_177 (54) = happyShift action_65
action_177 (55) = happyShift action_6
action_177 (56) = happyShift action_7
action_177 (57) = happyShift action_8
action_177 (59) = happyShift action_66
action_177 (60) = happyShift action_67
action_177 (61) = happyShift action_68
action_177 (63) = happyShift action_69
action_177 (82) = happyShift action_71
action_177 (16) = happyGoto action_45
action_177 (17) = happyGoto action_46
action_177 (20) = happyGoto action_154
action_177 (21) = happyGoto action_182
action_177 (25) = happyGoto action_51
action_177 (26) = happyGoto action_52
action_177 (27) = happyGoto action_53
action_177 _ = happyReduce_38

action_178 (30) = happyShift action_54
action_178 (32) = happyShift action_36
action_178 (40) = happyShift action_55
action_178 (41) = happyShift action_56
action_178 (42) = happyShift action_4
action_178 (43) = happyShift action_57
action_178 (44) = happyShift action_58
action_178 (45) = happyShift action_59
action_178 (47) = happyShift action_5
action_178 (48) = happyShift action_60
action_178 (50) = happyShift action_61
action_178 (51) = happyShift action_62
action_178 (52) = happyShift action_63
action_178 (53) = happyShift action_64
action_178 (54) = happyShift action_65
action_178 (55) = happyShift action_6
action_178 (56) = happyShift action_7
action_178 (57) = happyShift action_8
action_178 (59) = happyShift action_66
action_178 (60) = happyShift action_67
action_178 (61) = happyShift action_68
action_178 (63) = happyShift action_69
action_178 (81) = happyShift action_70
action_178 (82) = happyShift action_71
action_178 (15) = happyGoto action_44
action_178 (16) = happyGoto action_45
action_178 (17) = happyGoto action_46
action_178 (19) = happyGoto action_181
action_178 (20) = happyGoto action_49
action_178 (22) = happyGoto action_50
action_178 (25) = happyGoto action_51
action_178 (26) = happyGoto action_52
action_178 (27) = happyGoto action_53
action_178 _ = happyFail

action_179 _ = happyReduce_48

action_180 _ = happyReduce_63

action_181 _ = happyReduce_41

action_182 (31) = happyShift action_183
action_182 _ = happyFail

action_183 (30) = happyShift action_54
action_183 (32) = happyShift action_36
action_183 (40) = happyShift action_55
action_183 (41) = happyShift action_56
action_183 (42) = happyShift action_4
action_183 (43) = happyShift action_57
action_183 (44) = happyShift action_58
action_183 (45) = happyShift action_59
action_183 (47) = happyShift action_5
action_183 (48) = happyShift action_60
action_183 (50) = happyShift action_61
action_183 (51) = happyShift action_62
action_183 (52) = happyShift action_63
action_183 (53) = happyShift action_64
action_183 (54) = happyShift action_65
action_183 (55) = happyShift action_6
action_183 (56) = happyShift action_7
action_183 (57) = happyShift action_8
action_183 (59) = happyShift action_66
action_183 (60) = happyShift action_67
action_183 (61) = happyShift action_68
action_183 (63) = happyShift action_69
action_183 (81) = happyShift action_70
action_183 (82) = happyShift action_71
action_183 (15) = happyGoto action_44
action_183 (16) = happyGoto action_45
action_183 (17) = happyGoto action_46
action_183 (19) = happyGoto action_184
action_183 (20) = happyGoto action_49
action_183 (22) = happyGoto action_50
action_183 (25) = happyGoto action_51
action_183 (26) = happyGoto action_52
action_183 (27) = happyGoto action_53
action_183 _ = happyFail

action_184 _ = happyReduce_43

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (TokVarIdent happy_var_2)) `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Fdecl happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (TokVarIdent happy_var_2)) `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Fdefn happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyTerminal (TokVarIdent happy_var_2))
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn7
		 (Param happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  8 happyReduction_9
happyReduction_9  =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 _
	_
	 =  HappyAbsSyn9
		 ([]
	)

happyReduce_12 = happyReduce 4 9 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 10 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyTerminal (TokVarIdent happy_var_3)) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Typedef happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 _
	(HappyTerminal (TokStructIdent happy_var_2))
	_
	 =  HappyAbsSyn11
		 (Sdecl happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 12 happyReduction_15
happyReduction_15 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStructIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Sdef happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 _
	(HappyTerminal (TokVarIdent happy_var_2))
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Field happy_var_1 happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  14 happyReduction_17
happyReduction_17  =  HappyAbsSyn14
		 ([]
	)

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn16
		 (CInt
	)

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn16
		 (CBool
	)

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyTerminal (TokTypeIdent happy_var_1))
	 =  HappyAbsSyn16
		 (CTypeIdent happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn16
		 (CVoid
	)

happyReduce_24 = happySpecReduce_2  16 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (CPtr happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 _
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (CArray happy_var_1
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  16 happyReduction_26
happyReduction_26 (HappyTerminal (TokStructIdent happy_var_2))
	_
	 =  HappyAbsSyn16
		 (CStruct happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 17 happyReduction_27
happyReduction_27 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	(HappyTerminal (TokAsgnop happy_var_3)) `HappyStk`
	(HappyTerminal (TokVarIdent happy_var_2)) `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (checkDeclAsgn happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_2  17 happyReduction_28
happyReduction_28 (HappyTerminal (TokVarIdent happy_var_2))
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (JustDecl happy_var_1 happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  18 happyReduction_29
happyReduction_29  =  HappyAbsSyn18
		 ([]
	)

happyReduce_30 = happySpecReduce_2  18 happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  19 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (Simp happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn19
		 (Ctrl happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  19 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn19
		 (Blk happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  20 happyReduction_34
happyReduction_34 (HappyAbsSyn25  happy_var_3)
	(HappyTerminal (TokAsgnop happy_var_2))
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn20
		 (formAsgn happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  20 happyReduction_35
happyReduction_35 (HappyTerminal (TokPostop happy_var_2))
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn20
		 (formPost happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  20 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 (Decl happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn20
		 (Exp happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  21 happyReduction_38
happyReduction_38  =  HappyAbsSyn21
		 (Epsilon
	)

happyReduce_39 = happySpecReduce_1  21 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 (Simpopt happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 22 happyReduction_40
happyReduction_40 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (If happy_var_3 happy_var_5 Eps
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 7 22 happyReduction_41
happyReduction_41 ((HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (If happy_var_3 happy_var_5 (Else happy_var_7)
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 5 22 happyReduction_42
happyReduction_42 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 9 22 happyReduction_43
happyReduction_43 ((HappyAbsSyn19  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (For happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  22 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (Ret happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  22 happyReduction_45
happyReduction_45 _
	_
	 =  HappyAbsSyn22
		 (RetVoid
	)

happyReduce_46 = happyReduce 5 22 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Assert happy_var_3
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_0  23 happyReduction_47
happyReduction_47  =  HappyAbsSyn23
		 ([]
	)

happyReduce_48 = happySpecReduce_3  23 happyReduction_48
happyReduction_48 (HappyAbsSyn23  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2 : happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  24 happyReduction_49
happyReduction_49 _
	_
	 =  HappyAbsSyn24
		 ([]
	)

happyReduce_50 = happyReduce 4 24 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_3  25 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  25 happyReduction_52
happyReduction_52 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  25 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn25
		 (CTrue
	)

happyReduce_54 = happySpecReduce_1  25 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn25
		 (CFalse
	)

happyReduce_55 = happySpecReduce_1  25 happyReduction_55
happyReduction_55 (HappyTerminal (TokVarIdent happy_var_1))
	 =  HappyAbsSyn25
		 (Var happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  25 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn25
		 (NULL
	)

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  25 happyReduction_58
happyReduction_58 (HappyAbsSyn24  happy_var_2)
	(HappyTerminal (TokVarIdent happy_var_1))
	 =  HappyAbsSyn25
		 (Call happy_var_1 happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  25 happyReduction_59
happyReduction_59 (HappyTerminal (TokVarIdent happy_var_3))
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (Dot happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  25 happyReduction_60
happyReduction_60 (HappyTerminal (TokVarIdent happy_var_3))
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4 25 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Alloc happy_var_3
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_2  25 happyReduction_62
happyReduction_62 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (PointerStar happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 6 25 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (AllocArray happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 4 25 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ArrayAccess happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  26 happyReduction_65
happyReduction_65 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  26 happyReduction_66
happyReduction_66 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  26 happyReduction_67
happyReduction_67 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Mul happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  26 happyReduction_68
happyReduction_68 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  26 happyReduction_69
happyReduction_69 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Mod happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  26 happyReduction_70
happyReduction_70 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary ShiftL happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  26 happyReduction_71
happyReduction_71 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary ShiftR happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  26 happyReduction_72
happyReduction_72 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Less happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  26 happyReduction_73
happyReduction_73 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Greater happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  26 happyReduction_74
happyReduction_74 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Leq happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  26 happyReduction_75
happyReduction_75 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Geq happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  26 happyReduction_76
happyReduction_76 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Neq happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  26 happyReduction_77
happyReduction_77 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary BAnd happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  26 happyReduction_78
happyReduction_78 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary BXor happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  26 happyReduction_79
happyReduction_79 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary BOr happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  26 happyReduction_80
happyReduction_80 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary LAnd happy_var_1 happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  26 happyReduction_81
happyReduction_81 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary LOr happy_var_1 happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  26 happyReduction_82
happyReduction_82 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Binary Eq happy_var_1 happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_2  26 happyReduction_83
happyReduction_83 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Unary Not happy_var_2
	)
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_2  26 happyReduction_84
happyReduction_84 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Unary Flip happy_var_2
	)
happyReduction_84 _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  26 happyReduction_85
happyReduction_85 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Unary Neg happy_var_2
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happyReduce 5 26 happyReduction_86
happyReduction_86 ((HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Ternary happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_87 = happySpecReduce_1  27 happyReduction_87
happyReduction_87 (HappyTerminal (TokDec happy_var_1))
	 =  HappyAbsSyn27
		 (checkDec happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  27 happyReduction_88
happyReduction_88 (HappyTerminal (TokHex happy_var_1))
	 =  HappyAbsSyn27
		 (checkHex happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 84 84 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokLSquare -> cont 28;
	TokRSquare -> cont 29;
	TokLParen -> cont 30;
	TokRParen -> cont 31;
	TokLBrace -> cont 32;
	TokRBrace -> cont 33;
	TokComma -> cont 34;
	TokSemi -> cont 35;
	TokColon -> cont 36;
	TokQuestion -> cont 37;
	TokDot -> cont 38;
	TokArrow -> cont 39;
	TokAlloc -> cont 40;
	TokAllocArr -> cont 41;
	TokStruct -> cont 42;
	TokDec happy_dollar_dollar -> cont 43;
	TokHex happy_dollar_dollar -> cont 44;
	TokVarIdent happy_dollar_dollar -> cont 45;
	TokStructIdent happy_dollar_dollar -> cont 46;
	TokTypeIdent happy_dollar_dollar -> cont 47;
	TokIf -> cont 48;
	TokElse -> cont 49;
	TokWhile -> cont 50;
	TokFor -> cont 51;
	TokReturn -> cont 52;
	TokTrue -> cont 53;
	TokFalse -> cont 54;
	TokInt -> cont 55;
	TokBool -> cont 56;
	TokVoid -> cont 57;
	TokTypedef -> cont 58;
	TokMinus -> cont 59;
	TokNot -> cont 60;
	TokFlip -> cont 61;
	TokPlus -> cont 62;
	TokTimes -> cont 63;
	TokDiv -> cont 64;
	TokMod -> cont 65;
	TokLShift -> cont 66;
	TokRShift -> cont 67;
	TokLess -> cont 68;
	TokGreater -> cont 69;
	TokGeq -> cont 70;
	TokLeq -> cont 71;
	TokNeq -> cont 72;
	TokBAnd -> cont 73;
	TokBXor -> cont 74;
	TokBOr -> cont 75;
	TokLAnd -> cont 76;
	TokLOr -> cont 77;
	TokEquality -> cont 78;
	TokAsgnop happy_dollar_dollar -> cont 79;
	TokPostop happy_dollar_dollar -> cont 80;
	TokAssert -> cont 81;
	TokNULL -> cont 82;
	TokReserved -> cont 83;
	_ -> happyError' (tk:tks)
	}

happyError_ 84 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure    = return
    a <*> b = (fmap id a) <*> b
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseFdefnTokens tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError t = error ("ParseFdefn Error: " ++ show t)

convertToLvalue :: Exp -> Lvalue
convertToLvalue (Var ident) = Variable ident
convertToLvalue (Dot lvalue ident) = LvalueDot (convertToLvalue lvalue) ident
convertToLvalue (Arrow lvalue ident) = LvalueArrow (convertToLvalue lvalue) ident
convertToLvalue (PointerStar lvalue) = LvaluePointerStar (convertToLvalue lvalue)
convertToLvalue (ArrayAccess lvalue expr) =
  LvalueArrayAccess (convertToLvalue lvalue) expr
convertToLvalue other = error("Not an lvalue:" ++ show other)

formAsgn :: Exp -> Asnop -> Exp -> Simp
formAsgn e1 op e2 =
  Asgn (convertToLvalue e1) op e2

formPost :: Exp -> Postop -> Simp
formPost e1 op =
  Post (convertToLvalue e1) op

checkDeclAsgn :: CType -> Ident -> Asnop -> Exp -> Decl
checkDeclAsgn t v op e =
  case op of 
    Equal -> DeclAsgn t v e
    _ -> error "Invalid assignment operator on a declaration"

checkDec n = if (n > 2^31) then error "Decimal too big" else Const n
checkHex n = if (n >= 2^32) then error "Hex too big" else Const n
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
