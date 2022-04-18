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
                    # 'scn_mv_01_wave_activation_init', \
                    # 'scn_mv_02_wave_activated', \
                    # 'scn_mv_03_pawn_pass_through', \
                    # 'scn_mv_04_wave_activating_rook', \
                    # 'scn_mv_05_rook_activated', \
                    # 'scn_mv_06_rook_captures', \
                    # 'scn_mv_07_wave_no_activating_blocked_piece', \
                    # 'scn_mv_08_bishop_activating_wave', \
                    'scn_mv_09_wave_activated_by_bishop', \
                    # 'scn_mv_10_knight_activating_wave', \
                    'scn_mv_11_wave_activated_by_knight', \
                    # 'scn_mv_12_king_activating_wave', \
                    # 'scn_mv_13_wave_activated_by_king', \
                ]
