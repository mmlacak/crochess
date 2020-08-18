#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# from scene import Scene
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
# from scene_age_of_aquarius import SceneAgeOfAquariusMixin
# from scene_mirandas_veil import SceneMirandasVeilMixin
# from scene_nineteen import SceneNineteenMixin
# from scene_hemeras_dawn import SceneHemerasDawnMixin
# from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
# from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin


# class SceneMix(SceneCroatianTiesMixin, \
#                SceneMayanAscendancyMixin, \
#                SceneAgeOfAquariusMixin, \
#                SceneMirandasVeilMixin, \
#                SceneNineteenMixin, \
#                SceneHemerasDawnMixin, \
#                SceneTamoanchanRevisitedMixin, \
#                SceneConquestOfTlalocanMixin):

class SceneMix(SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin):

    def _get_recent_scene_method_names(self):
        return  [
                    # 'scn_ma_01_pyramid_activation_init', \
                    # 'scn_ma_02_pyramid_activated', \
                    # 'scn_ma_03_pyramid_activation_end', \
                    # 'scn_ma_04_pyramid_activation_by_pawn', \
                    # 'scn_ma_05_promo_init', \
                    # 'scn_ma_06_promo_pyramid_activated', \
                    # 'scn_ma_07_promo_end', \
                    # 'scn_ma_08_conversion_init', \
                    # 'scn_ma_09_conversion_pyramid_activated', \
                    # 'scn_ma_10_conversion_end', \
                    # 'scn_ma_11_convert_rook_castling_init', \
                    # 'scn_ma_12_convert_rook_castling_end', \
                    # 'scn_ma_13_convert_rook_castling', \
                    # 'scn_ma_14_convert_pawn_rush_init', \
                    # 'scn_ma_15_convert_pawn_rush_end', \
                    # 'scn_ma_16_cascading_init', \
                    # 'scn_ma_17_cascading_pyramid_1_activated', \
                    # 'scn_ma_18_cascading_pyramid_2_activated', \
                    # 'scn_ma_19_cascading_end', \
                    'scn_ma_20_pyramid_vs_king', \
                    'scn_ma_21_pyramid_vs_bishop', \
                ]

    def _get_all_scene_method_names(self):
        return [ n for n in dir(self) if n.startswith('scn_') ] # or n.startswith('move_')

    def _get_attributes(self, names):
        return [ getattr(self, a) for a in names ]

    def get_recent_scene_methods(self):
        return self._get_attributes( self._get_recent_scene_method_names() )

    def get_all_scene_methods(self):
        return self._get_attributes( self._get_all_scene_method_names() )
