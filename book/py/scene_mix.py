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
                    # 'scn_hd_24_grenadier_complete_extended_pattern', \
                    # 'scn_hd_25_grenadier_activated', \
                    # 'scn_hd_26_grenadier_close_quarters_activation', \
                    # 'scn_hd_27_grenadier_close_quarters_activated', \
                    # 'scn_hd_28_grenadier_activating_wave_step_field', \
                    # 'scn_hd_29_grenadier_activated_wave_step_field', \
                    # 'scn_hd_30_grenadier_activating_wave_capture_field', \
                    # 'scn_hd_31_grenadier_activated_wave_capture_field', \
                    # 'scn_hd_32_grenadier_en_passant', \
                    # 'scn_hd_33_grenadier_en_passant_self_extended', \
                    'scn_hd_34_grenadier_initial_positions', \
                ]
