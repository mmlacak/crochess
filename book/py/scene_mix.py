#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin
from scene_discovery import SceneDiscoveryMixin
from scene_one import SceneOneMixin


class SceneMix(SceneMixin, \
               SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin, \
               SceneNineteenMixin, \
               SceneHemerasDawnMixin, \
               SceneTamoanchanRevisitedMixin, \
               SceneConquestOfTlalocanMixin, \
               SceneDiscoveryMixin, \
               SceneOneMixin):

    # overrides
    def _get_recent_scene_method_names(self):
        return  [
                    # 'scn_cot_09_own_shaman_is_divergent_init', \
                    # 'scn_cot_10_own_shaman_is_divergent_end', \
                    # 'scn_cot_13_diverging_pawn_init', \
                    # 'scn_cot_14_diverging_pawn_end', \
                    # 'scn_cot_15_diverging_rushing_pawn', \
                    # 'scn_cot_16_diverging_unicorn_init', \
                    # 'scn_cot_17_diverging_unicorn_end', \
                    # 'scn_cot_11_diverging_activated_piece_init', \
                    # 'scn_cot_12_diverging_activated_piece_end', \
                    # 'scn_cot_19_activated_unicorn_divergence_init', \
                    # 'scn_cot_20_activated_unicorn_divergence_end', \
                    # 'scn_cot_21_centaur_cannot_diverge', \
                    # 'scn_cot_22_serpent_cannot_diverge', \
                    # 'scn_cot_23_diverging_shaman_init', \
                    # 'scn_cot_24_diverging_shaman_steps', \
                    # 'scn_cot_25_diverging_shaman_captures', \
                    'scn_cot_26_diverging_shaman_from_opponents', \
                    # 'scn_cot_27_wave_divergence_init', \
                    # 'scn_cot_28_wave_divergence_1', \
                    # 'scn_cot_29_wave_cannot_diverge_if_activated_by_unicorn', \
                    # 'scn_cot_30_wave_cannot_diverge_if_activated_by_centaur', \
                    # 'scn_cot_31_wave_cannot_diverge_if_activated_by_serpent', \
                    # 'scn_cot_32_multiple_divergences', \
                    # 'scn_cot_33_diverging_opponents_pieces', \
                ]
