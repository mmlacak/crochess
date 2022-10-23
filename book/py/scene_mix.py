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
                    # 'scn_mv_10_bishop_activating_wave', \
                    # 'scn_mv_16_wave_activation_by_step_pawn', \
                    # 'scn_mv_07_wave_is_transparent', \
                    # 'scn_mv_08_wave_cant_be_pinned', \
                    # 'scn_mv_37_pinned_piece_cascaded_init', \
                    # 'scn_mv_38_pinned_piece_cascaded_1', \
                    # 'scn_mv_39_pinned_piece_cascaded_end', \
                    # 'scn_mv_40_pinned_piece_cascaded_2', \
                    'scn_n_20_only_one_queen', \
                ]
